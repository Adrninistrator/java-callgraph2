package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.*;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.util.CommonUtil;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

// 入口类
public class JCallGraph {

    public static final int INIT_SIZE_100 = 100;
    public static final int INIT_SIZE_500 = 500;
    public static final int INIT_SIZE_1000 = 1000;

    private static final String RUNNABLE_CLASS_NAME = Runnable.class.getName();
    private static final String CALLABLE_CLASS_NAME = Callable.class.getName();
    private static final String THREAD_CLASS_NAME = Thread.class.getName();

    private Map<String, Set<String>> calleeMethodMapGlobal;
    private Map<String, ClassInterfaceMethodInfo> classInterfaceMethodInfoMap;
    private Map<String, List<String>> interfaceMethodWithArgsMap;
    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> threadChildClassMap;
    private Map<String, Set<String>> methodAnnotationMap;
    private Set<String> extendsClassesSet;
    private Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap;
    private Map<String, List<String>> childrenClassInfoMap;
    private CallIdCounter callIdCounter = CallIdCounter.newInstance();
    private List<CustomCodeParserInterface> customCodeParserList = new ArrayList<>();

    private int jarNum = 0;

    public static void main(String[] args) {
        JCallGraph jCallGraph = new JCallGraph();
        jCallGraph.run(args);
    }

    public void addCustomCodeParser(CustomCodeParserInterface customCodeParser) {
        customCodeParserList.add(customCodeParser);
    }

    public boolean run(String[] args) {
        // calleeMethodMapGlobal在处理所有的jar包时都需要使用，只初始化一次
        calleeMethodMapGlobal = new HashMap<>(INIT_SIZE_1000);

        String outputFilePath = System.getProperty("output.file");
        if (outputFilePath == null || outputFilePath.isEmpty()) {
            System.err.println("### 请使用\"-Doutput.file=xxx\"指定输出文件路径");
            return false;
        }

        Map<String, Integer> filePathSet = new HashMap<>(args.length);

        String annotationOutputFilePath = outputFilePath + "-annotation.txt";
        System.out.println("将方法注解信息写入文件: " + annotationOutputFilePath);

        try (BufferedWriter resultWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFilePath)));
             BufferedWriter annotationOut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(annotationOutputFilePath),
                     StandardCharsets.UTF_8))) {
            for (String arg : args) {
                String jarFilePath = CommonUtil.getCanonicalPath(arg);
                if (jarFilePath == null) {
                    System.err.println("### getCanonicalPath失败: " + arg);
                    return false;
                }

                if (filePathSet.get(jarFilePath) != null) {
                    System.out.println(arg + " 跳过jar文件: " + jarFilePath);
                    continue;
                }

                filePathSet.put(jarFilePath, ++jarNum);

                System.out.println(arg + " 处理jar文件: " + jarFilePath);
                File jarFile = new File(jarFilePath);

                if (!jarFile.exists()) {
                    System.err.println("### jar文件 " + jarFilePath + " 不存在");
                }

                // 调用自定义接口实现类的方法
                for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
                    customCodeParser.handleJar(jarFilePath);
                }

                // 处理一个jar包
                if (!handleOneJar(jarFile, jarFilePath, resultWriter, annotationOut)) {
                    return false;
                }
            }

            return true;
        } catch (IOException e) {
            System.err.println("### 出现异常: " + e.getMessage());
            e.printStackTrace();
            return false;
        }
    }

    private void init() {
        classInterfaceMethodInfoMap = new HashMap<>(INIT_SIZE_100);
        interfaceMethodWithArgsMap = new HashMap<>(INIT_SIZE_100);
        runnableImplClassMap = new HashMap<>(INIT_SIZE_100);
        callableImplClassMap = new HashMap<>(INIT_SIZE_100);
        threadChildClassMap = new HashMap<>(INIT_SIZE_100);
        methodAnnotationMap = new HashMap<>(INIT_SIZE_100);
        extendsClassesSet = new HashSet<>(INIT_SIZE_500);
        extendsClassMethodInfoMap = new HashMap<>(INIT_SIZE_500);
        childrenClassInfoMap = new HashMap<>(INIT_SIZE_500);
    }

    // 处理一个jar包
    private boolean handleOneJar(File jarFile, String jarFilePath, BufferedWriter resultWriter, BufferedWriter annotationOut) throws IOException {
        try (JarFile jar = new JarFile(jarFile)) {
            writeResult(resultWriter, "J:" + jarNum + " " + jarFilePath);
            writeResult(resultWriter, JavaCGConstants.NEW_LINE);

            // 初始化
            init();

            // 对Class进行预处理
            if (!preHandleClasses(jarFilePath, jarFile)) {
                return false;
            }

            for (Enumeration<JarEntry> enumeration = jar.entries(); enumeration.hasMoreElements(); ) {
                JarEntry jarEntry = enumeration.nextElement();
                if (!jarEntry.isDirectory()) {
                    if (jarEntry.getName().endsWith(".class")) {
                        // 处理一个class文件
                        handleOneClass(jarFilePath, jarEntry, resultWriter);
                    }
                }
            }

            // 将接口中的抽象方法加到抽象父类中
            if (!addInterfaceMethod4SuperClass()) {
                return false;
            }

            // 记录父类调用子类方法，及子类调用父类方法
            if (!recordExtendsClassMethod(resultWriter)) {
                return false;
            }

            // 记录接口调用实现类方法
            recordInterfaceCallClassMethod(resultWriter);

            // 记录方法注解信息
            recordMethodAnnotationInfo(annotationOut);

            return true;
        }
    }

    // 处理一个class文件
    private void handleOneClass(String jarFilePath, JarEntry jarEntry, BufferedWriter resultWriter) throws IOException {
        ClassParser cp = new ClassParser(jarFilePath, jarEntry.getName());
        JavaClass javaClass = cp.parse();

        System.out.println("处理Class: " + javaClass.getClassName());

        if (javaClass.isClass() && extendsClassesSet.contains(javaClass.getClassName())) {
            // 查找涉及继承的类的信息
            findExtendsClassesInfo(javaClass);
        }

        ClassVisitor classVisitor = new ClassVisitor(javaClass);
        classVisitor.setCalleeMethodMap(calleeMethodMapGlobal);
        classVisitor.setRunnableImplClassMap(runnableImplClassMap);
        classVisitor.setCallableImplClassMap(callableImplClassMap);
        classVisitor.setThreadChildClassMap(threadChildClassMap);
        classVisitor.setMethodAnnotationMap(methodAnnotationMap);
        classVisitor.setCallIdCounter(callIdCounter);
        classVisitor.setCustomCodeParserList(customCodeParserList);
        classVisitor.start();

        List<MethodCallDto> methodCalls = classVisitor.methodCalls();
        for (MethodCallDto methodCallDto : methodCalls) {
            writeResult(resultWriter, methodCallDto.getMethodCall());
            if (methodCallDto.getSourceLine() != JavaCGConstants.NONE_LINE_NUMBER) {
                writeResult(resultWriter, " " + methodCallDto.getSourceLine());
                writeResult(resultWriter, " " + jarNum);
            }
            writeResult(resultWriter, JavaCGConstants.NEW_LINE);
        }

        // 调用自定义接口实现类的方法
        for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
            customCodeParser.handleClass(javaClass);
        }
    }

    // 将接口中的抽象方法加到抽象父类中
    private boolean addInterfaceMethod4SuperClass() {
        for (Map.Entry<String, List<String>> childrenClassInfoEntry : childrenClassInfoMap.entrySet()) {
            String superClassName = childrenClassInfoEntry.getKey();
            ExtendsClassMethodInfo extendsClassMethodInfo = extendsClassMethodInfoMap.get(superClassName);
            if (extendsClassMethodInfo == null) {
                // 其他jar包中的Class可以找到，但是找不到它们的方法，是正常的情况
                continue;
            }

            if (!extendsClassMethodInfo.isAbstractClass()) {
                continue;
            }

            ClassInterfaceMethodInfo classInterfaceMethodInfo = classInterfaceMethodInfoMap.get(superClassName);
            if (classInterfaceMethodInfo == null) {
                continue;
            }

            Map<String, MethodAttribute> methodAttributeMap = extendsClassMethodInfo.getMethodAttributeMap();
            MethodAttribute methodAttribute = new MethodAttribute();
            methodAttribute.setAbstractMethod(true);
            methodAttribute.setPublicMethod(true);
            methodAttribute.setProtectedMethod(false);

            List<String> interfaceNameList = classInterfaceMethodInfo.getInterfaceNameList();
            for (String interfaceName : interfaceNameList) {
                List<String> interfaceMethodWithArgsList = interfaceMethodWithArgsMap.get(interfaceName);
                if (interfaceMethodWithArgsList == null) {
                    continue;
                }

                for (String interfaceMethodWithArgs : interfaceMethodWithArgsList) {
                    methodAttributeMap.putIfAbsent(interfaceMethodWithArgs, methodAttribute);
                }
            }
        }

        return true;
    }

    // 记录父类调用子类方法，及子类调用父类方法
    private boolean recordExtendsClassMethod(BufferedWriter resultWriter) throws IOException {
        Set<String> topSuperClassNameSet = new HashSet<>();

        // 得到最顶层父类名称
        for (Map.Entry<String, ExtendsClassMethodInfo> extendsClassMethodInfoEntry : extendsClassMethodInfoMap.entrySet()) {
            String className = extendsClassMethodInfoEntry.getKey();
            ExtendsClassMethodInfo extendsClassMethodInfo = extendsClassMethodInfoEntry.getValue();
            String superClassName = extendsClassMethodInfo.getSuperClassName();
            if (superClassName.startsWith("java.")) {
                topSuperClassNameSet.add(className);
            }
        }

        for (String topSuperClassName : topSuperClassNameSet) {
            // 处理一个顶层父类
            if (!handleOneTopSuperClass(topSuperClassName, resultWriter)) {
                return false;
            }
        }
        return true;
    }

    // 处理一个顶层父类
    private boolean handleOneTopSuperClass(String topSuperClassName, BufferedWriter resultWriter) throws IOException {
        System.out.println("处理一个顶层父类: " + topSuperClassName);
        List<TmpNode4ExtendsClassMethod> tmpNodeList = new ArrayList<>();
        int currentLevel = 0;

        // 初始化节点列表
        TmpNode4ExtendsClassMethod topNode = TmpNode4ExtendsClassMethod.genInstance(topSuperClassName, -1);
        tmpNodeList.add(topNode);

        // 开始循环
        while (true) {
            TmpNode4ExtendsClassMethod currentNode = tmpNodeList.get(currentLevel);
            List<String> childrenClassInfoList = childrenClassInfoMap.get(currentNode.getSuperClassName());
            if (childrenClassInfoList == null) {
                System.err.println("### 未找到顶层父类: " + currentNode.getSuperClassName());
                return false;
            }

            int currentChildClassIndex = currentNode.getChildClassIndex() + 1;
            if (currentChildClassIndex >= childrenClassInfoList.size()) {
                if (currentLevel == 0) {
                    return true;
                }
                currentLevel--;
                continue;
            }

            // 处理当前的子类
            String childClassName = childrenClassInfoList.get(currentChildClassIndex);

            // 处理父类和子类的方法调用
            if (!handleSuperAndChildClass(currentNode.getSuperClassName(), childClassName, resultWriter)) {
                return false;
            }

            // 处理下一个子类
            currentNode.setChildClassIndex(currentChildClassIndex);

            List<String> nextChildClassList = childrenClassInfoMap.get(childClassName);
            if (nextChildClassList == null) {
                // 当前的子类下没有子类
                continue;
            }

            // 当前的子类下有子类
            currentLevel++;

            if (currentLevel + 1 > tmpNodeList.size()) {
                TmpNode4ExtendsClassMethod nextNode = TmpNode4ExtendsClassMethod.genInstance(childClassName, -1);
                tmpNodeList.add(nextNode);
            } else {
                TmpNode4ExtendsClassMethod nextNode = tmpNodeList.get(currentLevel);
                nextNode.setSuperClassName(childClassName);
                nextNode.setChildClassIndex(-1);
            }
        }
    }

    // 处理父类和子类的方法调用
    private boolean handleSuperAndChildClass(String superClassName, String childClassName, BufferedWriter resultWriter) throws IOException {
        ExtendsClassMethodInfo superClassMethodInfo = extendsClassMethodInfoMap.get(superClassName);
        if (superClassMethodInfo == null) {
            System.err.println("### 未找到父类信息: " + superClassName);
            return false;
        }

        ExtendsClassMethodInfo childClassMethodInfo = extendsClassMethodInfoMap.get(childClassName);
        if (childClassMethodInfo == null) {
            System.err.println("### 未找到子类信息: " + childClassName);
            return false;
        }

        Map<String, MethodAttribute> superMethodAttributeMap = superClassMethodInfo.getMethodAttributeMap();
        Map<String, MethodAttribute> childMethodAttributeMap = childClassMethodInfo.getMethodAttributeMap();

        for (Map.Entry<String, MethodAttribute> superMethodAttributeEntry : superMethodAttributeMap.entrySet()) {
            String superMethodWithArgs = superMethodAttributeEntry.getKey();
            MethodAttribute superMethodAttribute = superMethodAttributeEntry.getValue();
            if (superMethodAttribute.isAbstractMethod()) {
                // 处理父类抽象方法
                childMethodAttributeMap.putIfAbsent(superMethodWithArgs, superMethodAttribute);
                // 添加父类调用子类的方法调用
                String superCallChildClassMethod = String.format("M:%d %s:%s (%s)%s:%s %d", callIdCounter.addAndGet(), superClassName, superMethodWithArgs,
                        CallTypeEnum.CTE_SCC.getType(), childClassName, superMethodWithArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
                writeResult(resultWriter, superCallChildClassMethod);
                writeResult(resultWriter, " " + jarNum);
                writeResult(resultWriter, JavaCGConstants.NEW_LINE);
                continue;
            }
            if (superMethodAttribute.isPublicMethod() || superMethodAttribute.isProtectedMethod()) {
                // 父类的public/protected且非抽象方法
                if (childMethodAttributeMap.get(superMethodWithArgs) != null) {
                    continue;
                }
                Set<String> childCalleeMethodWithArgsSet = calleeMethodMapGlobal.get(childClassName);
                if (!childClassMethodInfo.isAbstractClass() &&
                        (childCalleeMethodWithArgsSet == null || !childCalleeMethodWithArgsSet.contains(superMethodWithArgs))) {
                    continue;
                }

                childMethodAttributeMap.put(superMethodWithArgs, superMethodAttribute);

                // 添加子类调用父类方法
                String childCallSuperClassMethod = String.format("M:%d %s:%s (%s)%s:%s %d", callIdCounter.addAndGet(), childClassName, superMethodWithArgs,
                        CallTypeEnum.CTE_CCS.getType(), superClassName, superMethodWithArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
                writeResult(resultWriter, childCallSuperClassMethod);
                writeResult(resultWriter, " " + jarNum);
                writeResult(resultWriter, JavaCGConstants.NEW_LINE);
            }
        }
        return true;
    }

    // 记录接口调用实现类方法
    private void recordInterfaceCallClassMethod(BufferedWriter resultWriter) throws IOException {
        if (classInterfaceMethodInfoMap.isEmpty() || interfaceMethodWithArgsMap.isEmpty()) {
            return;
        }

        for (Map.Entry<String, ClassInterfaceMethodInfo> classMethodInfo : classInterfaceMethodInfoMap.entrySet()) {
            String className = classMethodInfo.getKey();
            ClassInterfaceMethodInfo classInterfaceMethodInfo = classMethodInfo.getValue();
            List<String> interfaceNameList = classInterfaceMethodInfo.getInterfaceNameList();

            // 找到在接口和实现类中都存在的，且有被调用的方法
            for (String interfaceName : interfaceNameList) {
                Set<String> calleeMethodWithArgsSet = calleeMethodMapGlobal.get(interfaceName);
                if (calleeMethodWithArgsSet == null) {
                    continue;
                }

                List<String> interfaceMethodWithArgsList = interfaceMethodWithArgsMap.get(interfaceName);
                if (interfaceMethodWithArgsList == null || interfaceMethodWithArgsList.isEmpty()) {
                    continue;
                }

                List<String> classMethodWithArgsList = classInterfaceMethodInfo.getMethodWithArgsList();
                for (String classMethodWithArgs : classMethodWithArgsList) {
                    if (!interfaceMethodWithArgsList.contains(classMethodWithArgs) || !calleeMethodWithArgsSet.contains(classMethodWithArgs)) {
                        continue;
                    }

                    String interfaceCallClassMethod = String.format("M:%d %s:%s (%s)%s:%s %d", callIdCounter.addAndGet(), interfaceName, classMethodWithArgs,
                            CallTypeEnum.CTE_ITF.getType(), className, classMethodWithArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
                    writeResult(resultWriter, interfaceCallClassMethod);
                    writeResult(resultWriter, " " + jarNum);
                    writeResult(resultWriter, JavaCGConstants.NEW_LINE);
                }
            }
        }
    }

    // 对Class进行预处理
    private boolean preHandleClasses(String jarFilePath, File jarFile) {
        try (JarFile jar = new JarFile(jarFile)) {
            for (Enumeration<JarEntry> enumeration = jar.entries(); enumeration.hasMoreElements(); ) {
                JarEntry jarEntry = enumeration.nextElement();
                if (!jarEntry.isDirectory()) {
                    if (jarEntry.getName().endsWith(".class")) {
                        // 对一个类文件进行预处理
                        preHandleOneFile(jarFilePath, jarEntry);
                    }

                    // 调用自定义接口实现类的方法
                    for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
                        customCodeParser.handleJarEntryFile(jar, jarEntry);
                    }
                }
            }
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    // 对一个类文件进行预处理
    private void preHandleOneFile(String jarFilePath, JarEntry jarEntry) throws IOException {
        ClassParser cp = new ClassParser(jarFilePath, jarEntry.getName());
        JavaClass javaClass = cp.parse();

        String className = javaClass.getClassName();
        if (javaClass.isClass()) {
            // 对一个Class进行预处理
            preHandleClass(javaClass);
        } else if (javaClass.isInterface()) {
            Method[] methods = javaClass.getMethods();
            if (methods != null && methods.length > 0 &&
                    interfaceMethodWithArgsMap.get(className) == null) {
                List<String> interfaceMethodWithArgsList = CommonUtil.genInterfaceAbstractMethodWithArgs(methods);
                interfaceMethodWithArgsMap.put(className, interfaceMethodWithArgsList);
            }
        }

        // 获得父类和子类信息
        String superClassName = javaClass.getSuperclassName();
        if (THREAD_CLASS_NAME.equals(superClassName)) {
            // 找到Thread的子类
            threadChildClassMap.put(javaClass.getClassName(), Boolean.FALSE);
        }

        if (!superClassName.startsWith("java.")) {
            extendsClassesSet.add(javaClass.getClassName());
            extendsClassesSet.add(superClassName);
        }

        // 调用自定义接口实现类的方法
        for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
            customCodeParser.preHandleClass(javaClass);
        }
    }

    // 对一个Class进行预处理
    private void preHandleClass(JavaClass javaClass) {
        String className = javaClass.getClassName();
        String[] interfaceNames = javaClass.getInterfaceNames();
        Method[] methods = javaClass.getMethods();

        if (interfaceNames != null && interfaceNames.length > 0 &&
                methods != null && methods.length > 0 &&
                classInterfaceMethodInfoMap.get(className) == null) {
            ClassInterfaceMethodInfo classInterfaceMethodInfo = new ClassInterfaceMethodInfo();

            List<String> interfaceNameList = new ArrayList<>(interfaceNames.length);
            interfaceNameList.addAll(Arrays.asList(interfaceNames));

            List<String> implClassMethodWithArgsList = CommonUtil.genImplClassMethodWithArgs(methods);
            classInterfaceMethodInfo.setInterfaceNameList(interfaceNameList);
            classInterfaceMethodInfo.setMethodWithArgsList(implClassMethodWithArgsList);

            classInterfaceMethodInfoMap.put(className, classInterfaceMethodInfo);

            if (!javaClass.isAbstract()) {
                if (interfaceNameList.contains(RUNNABLE_CLASS_NAME)) {
                    // 找到Runnable实现类
                    runnableImplClassMap.put(className, Boolean.FALSE);
                }
                if (interfaceNameList.contains(CALLABLE_CLASS_NAME)) {
                    // 找到Callable实现类
                    callableImplClassMap.put(className, Boolean.FALSE);
                }
            }
        }
    }

    // 查找涉及继承的类的信息
    private void findExtendsClassesInfo(JavaClass javaClass) {
        String className = javaClass.getClassName();
        if (extendsClassMethodInfoMap.get(className) != null) {
            return;
        }

        String superClassName = javaClass.getSuperclassName();
        if (!superClassName.startsWith("java.")) {
            // 将父类及其子类进行缓存，忽略以"java."开头的父类
            List<String> childrenClassInfoList = childrenClassInfoMap.get(superClassName);
            if (childrenClassInfoList == null) {
                List<String> newChildrenClassInfoList = new ArrayList<>();
                newChildrenClassInfoList.add(className);
                childrenClassInfoMap.put(superClassName, newChildrenClassInfoList);
            } else {
                childrenClassInfoList.add(className);
            }
        }

        // 将当前类的方法信息缓存
        ExtendsClassMethodInfo extendsClassMethodInfo = new ExtendsClassMethodInfo();
        extendsClassMethodInfo.setAbstractClass(javaClass.isAbstract());
        extendsClassMethodInfo.setSuperClassName(superClassName);
        Map<String, MethodAttribute> methodAttributeMap = new HashMap<>();

        Method[] methods = javaClass.getMethods();
        if (methods != null && methods.length > 0) {
            for (Method method : methods) {
                String methodName = method.getName();
                if (!methodName.startsWith("<") &&
                        !method.isStatic()
                        && (method.isAbstract() || method.isPublic() || method.isProtected())
                ) {
                    MethodAttribute methodAttribute = new MethodAttribute();
                    methodAttribute.setAbstractMethod(method.isAbstract());
                    methodAttribute.setPublicMethod(method.isPublic());
                    methodAttribute.setProtectedMethod(method.isProtected());

                    String methodWithArgs = methodName + CommonUtil.argumentList(method.getArgumentTypes());
                    methodAttributeMap.put(methodWithArgs, methodAttribute);
                }
            }
        }
        extendsClassMethodInfo.setMethodAttributeMap(methodAttributeMap);
        extendsClassMethodInfoMap.put(className, extendsClassMethodInfo);
    }

    // 记录方法注解信息
    private void recordMethodAnnotationInfo(BufferedWriter out) throws IOException {
        for (Map.Entry<String, Set<String>> entry : methodAnnotationMap.entrySet()) {
            String fullMethod = entry.getKey();
            Set<String> annotationSet = entry.getValue();
            for (String annotation : annotationSet) {
                String methodWithAnnotation = fullMethod + " " + annotation + JavaCGConstants.NEW_LINE;
                out.write(methodWithAnnotation);
            }
        }
    }

    // 将结果写到文件中
    private void writeResult(BufferedWriter resultWriter, String data) throws IOException {
        resultWriter.write(data);
    }
}
