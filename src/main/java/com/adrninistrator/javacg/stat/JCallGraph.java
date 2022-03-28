package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.*;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.enums.HandleJarResultEnum;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.util.HandleJarUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
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

    private static final String METHOD_CALL_FORMAT = JavaCGConstants.FILE_KEY_METHOD_PREFIX + "%d %s:%s (%s)%s:%s %d";

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

    /*
        是否需要记录所有的接口调用实现类，及子类调用父类方法
        默认不需要，即接口调用实现类，及子类调用父类方法，仅当调用方法有被其他方法调用时，才记录
        若为true，则不再需要使用calleeMethodMapGlobal；若为false，则需要使用calleeMethodMapGlobal
        需要保留该选项，因为有可能需要根据接口或父类展示所有的实现类或子类的操作
     */
    private boolean recordAll = false;

    // 保存当前的输出文件路径
    private String outputFilePath;

    // 保存当前输出的注解信息的文件路径
    private String annotationOutputFilePath;

    // 处理class文件时，缓存当前处理的文件的第一层目录名及对应jar包信息
    private String lastFirstDirName;
    private JarInfo lastJarInfo;

    public static void main(String[] args) {
        JCallGraph jCallGraph = new JCallGraph();
        jCallGraph.run(args);
    }

    // 记录所有的接口调用实现类，及子类调用父类方法
    public static void setRecordAll() {
        System.setProperty(JavaCGConstants.PROPERTY_RECORD_ALL, "1");
    }

    // 设置合并jar/war包中的class文件时，需要合并的包名
    public static void setMergeClassInJarPackage(String mergeClassInJarPackage) {
        System.setProperty(JavaCGConstants.PROPERTY_MERGE_CLASS_IN_JAR_PACKAGE, mergeClassInJarPackage);
    }

    // 添加自定义代码解析类
    public void addCustomCodeParser(CustomCodeParserInterface customCodeParser) {
        customCodeParserList.add(customCodeParser);
    }

    // 获取输出结果文件路径
    public String getOutputFilePath() {
        return outputFilePath;
    }

    // 获取注解相关信息输出结果文件路径
    public String getAnnotationOutputFilePath() {
        return annotationOutputFilePath;
    }

    public boolean run(String[] args) {
        if (args == null || args.length == 0) {
            System.err.println("请在执行参数中指定需要处理的jar包或目录列表，使用空格分隔");
            return false;
        }

        long startTime = System.currentTimeMillis();
        System.out.println("需要处理的jar包或目录:");
        for (String arg : args) {
            System.out.println(arg);
        }

        // calleeMethodMapGlobal在处理所有的jar包时都需要使用，只初始化一次
        calleeMethodMapGlobal = new HashMap<>(INIT_SIZE_1000);

        if (System.getProperty(JavaCGConstants.PROPERTY_RECORD_ALL) != null) {
            System.out.println("指定了需要记录所有的接口调用实现类，及子类调用父类方法");
            recordAll = true;
        }

        // 保存需要处理的jar包文件名及对应的序号
        Map<String, JarInfo> jarInfoMap = new HashMap<>(args.length);

        // 对指定的jar包进行处理
        String newJarFilePath = HandleJarUtil.handleJar(args, jarInfoMap);
        if (newJarFilePath == null) {
            return false;
        }

        System.out.println("实际处理的jar文件: " + newJarFilePath);

        outputFilePath = newJarFilePath + JavaCGConstants.EXT_TXT;
        annotationOutputFilePath = newJarFilePath + JavaCGConstants.FILE_FLAG_ANNOTATION + JavaCGConstants.EXT_TXT;
        System.out.println("写入文件: " + outputFilePath + " " + annotationOutputFilePath);

        try (BufferedWriter resultWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newJarFilePath + JavaCGConstants.EXT_TXT)));
             BufferedWriter annotationOut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(annotationOutputFilePath),
                     StandardCharsets.UTF_8))) {
            File jarFile = new File(newJarFilePath);

            // 调用自定义接口实现类的方法
            for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
                customCodeParser.handleJar(newJarFilePath);
            }

            // 处理一个jar包
            if (!handleOneJar(jarFile, newJarFilePath, resultWriter, annotationOut, jarInfoMap)) {
                return false;
            }

            long spendTime = System.currentTimeMillis() - startTime;
            System.out.println("执行完毕，耗时: " + (spendTime / 1000.0D) + " S");
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

        lastFirstDirName = null;
        lastJarInfo = null;
    }

    // 处理一个jar包
    private boolean handleOneJar(File jarFile, String jarFilePath, BufferedWriter resultWriter, BufferedWriter annotationOut, Map<String, JarInfo> jarInfoMap) {
        try (JarFile jar = new JarFile(jarFile)) {
            // 初始化
            init();

            // 对Class进行预处理
            if (!preHandleClasses(jarFilePath, jarFile)) {
                return false;
            }

            // 对Class进行第二次预处理
            if (!preHandleClasses2(jarFilePath, jarFile)) {
                return false;
            }

            // 处理当前jar包中的class文件
            Enumeration<JarEntry> enumeration = jar.entries();
            while (enumeration.hasMoreElements()) {
                JarEntry jarEntry = enumeration.nextElement();
                if (!jarEntry.isDirectory() && jarEntry.getName().toLowerCase().endsWith(JavaCGConstants.EXT_CLASS)) {
                    // 处理一个class文件
                    handleOneClass(jarFilePath, jarEntry, resultWriter, jarInfoMap);
                }
            }

            // 将接口中的抽象方法添加到抽象父类中
            addInterfaceMethod4SuperClass();

            // 记录父类调用子类方法，及子类调用父类方法
            if (!recordExtendsClassMethod(resultWriter)) {
                return false;
            }

            // 记录接口调用实现类方法
            recordInterfaceCallClassMethod(resultWriter);

            // 记录方法注解信息
            recordMethodAnnotationInfo(annotationOut);

            return true;
        } catch (Exception e) {
            System.err.println("处理jar包出现异常 " + jarFilePath);
            e.printStackTrace();
            return false;
        }
    }

    // 处理一个class文件
    private void handleOneClass(String jarFilePath, JarEntry jarEntry, BufferedWriter resultWriter, Map<String, JarInfo> jarInfoMap) {
        String jarEntryName = jarEntry.getName();
        try {

            // 获取当前处理的jar包信息
            HandleJarResultEnum handleJarResultEnum = handleCurrentJarInfo(jarInfoMap, jarEntryName);
            if (handleJarResultEnum == HandleJarResultEnum.HJRE_FAIL) {
                return;
            }

            if (handleJarResultEnum == HandleJarResultEnum.HJRE_FIRST) {
            /*
                第一次处理某个jar包
                向文件写入数据，内容为jar包信息
                格式为“J:[jar包序号] jar包文件路径]，或”D:[jar包序号] 目录路径“
             */
                writeResult(resultWriter, lastJarInfo.getJarType() + lastJarInfo.getJarNum() + " " + lastJarInfo.getJarPath());
            }

            ClassParser cp = new ClassParser(jarFilePath, jarEntryName);
            JavaClass javaClass = cp.parse();

            JavaCGUtil.debugPrint("处理Class: " + javaClass.getClassName());

            ClassVisitor classVisitor = new ClassVisitor(javaClass);
            classVisitor.setCalleeMethodMapGlobal(calleeMethodMapGlobal);
            classVisitor.setRunnableImplClassMap(runnableImplClassMap);
            classVisitor.setCallableImplClassMap(callableImplClassMap);
            classVisitor.setThreadChildClassMap(threadChildClassMap);
            classVisitor.setMethodAnnotationMap(methodAnnotationMap);
            classVisitor.setCallIdCounter(callIdCounter);
            classVisitor.setCustomCodeParserList(customCodeParserList);
            classVisitor.setRecordAll(recordAll);

            classVisitor.start();

            List<MethodCallDto> methodCalls = classVisitor.methodCalls();
            for (MethodCallDto methodCallDto : methodCalls) {
                String data = methodCallDto.getMethodCall();
                if (methodCallDto.getSourceLine() != JavaCGConstants.NONE_LINE_NUMBER) {
                    data = data + " " + methodCallDto.getSourceLine() + " " + lastJarInfo.getJarNum();
                }
                writeResult(resultWriter, data);
            }

            // 调用自定义接口实现类的方法
            for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
                customCodeParser.handleClass(javaClass);
            }
        } catch (Exception e) {
            System.err.println("处理class文件出现异常 " + jarEntryName);
            e.printStackTrace();
        }
    }

    // 获取当前处理的jar包信息
    private HandleJarResultEnum handleCurrentJarInfo(Map<String, JarInfo> jarInfoMap, String jarEntryName) {
        if (jarInfoMap.size() == 1) {
            // 只有一个jar包，从Map取值时使用默认key
            if (lastJarInfo == null) {
                // 第一次处理当前jar包
                for (Map.Entry<String, JarInfo> entry : jarInfoMap.entrySet()) {
                    lastJarInfo = entry.getValue();
                    break;
                }
                return HandleJarResultEnum.HJRE_FIRST;
            }
            // 不是第一次处理当前jar包
            return HandleJarResultEnum.HJRE_NOT_FIRST;
        }

        // jar包数量大于1个，从Map取值时使用当前JarEntry的第一层目录名称
        int index = jarEntryName.indexOf("/");
        if (index == -1) {
            System.err.println("JarEntry名称中不包含/ " + jarEntryName);
            return HandleJarResultEnum.HJRE_FAIL;
        }

        String firstDirName = jarEntryName.substring(0, index);
        if (lastFirstDirName != null && lastFirstDirName.equals(firstDirName)) {
            // 第一层目录名未变化时，使用缓存数据
            return HandleJarResultEnum.HJRE_NOT_FIRST;
        }
        lastFirstDirName = firstDirName;

        // 首次处理，或第一层目录名变化时，需要从Map获取
        lastJarInfo = jarInfoMap.get(firstDirName);
        if (lastJarInfo == null) {
            System.err.println("合并后的jar包中出现的名称未记录过: " + jarEntryName);
        }
        return HandleJarResultEnum.HJRE_FIRST;
    }

    // 将接口中的抽象方法加到抽象父类中
    private void addInterfaceMethod4SuperClass() {
        for (Map.Entry<String, List<String>> childrenClassInfoEntry : childrenClassInfoMap.entrySet()) {
            String superClassName = childrenClassInfoEntry.getKey();
            ExtendsClassMethodInfo extendsClassMethodInfo = extendsClassMethodInfoMap.get(superClassName);
            if (extendsClassMethodInfo == null || !extendsClassMethodInfo.isAbstractClass()) {
                // 为空的情况，对应其他jar包中的Class可以找到，但是找不到它们的方法，是正常的，不处理
                // 若不是抽象类则不处理
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
        JavaCGUtil.debugPrint("处理一个顶层父类: " + topSuperClassName);
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
                String superCallChildClassMethod = String.format(METHOD_CALL_FORMAT, callIdCounter.addAndGet(), superClassName, superMethodWithArgs,
                        CallTypeEnum.CTE_SCC.getType(), childClassName, superMethodWithArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
                writeResult(resultWriter, superCallChildClassMethod + " " + JavaCGConstants.DEFAULT_JAR_NUM);
            } else if (superMethodAttribute.isPublicMethod() || superMethodAttribute.isProtectedMethod()) {
                // 父类的public/protected且非抽象方法
                if (childMethodAttributeMap.get(superMethodWithArgs) != null) {
                    continue;
                }

                if (!childClassMethodInfo.isAbstractClass() && !recordAll) {
                    // 子类非抽象类，判断是否有被调用
                    Set<String> childCalleeMethodWithArgsSet = calleeMethodMapGlobal.get(childClassName);
                    if (childCalleeMethodWithArgsSet == null || !childCalleeMethodWithArgsSet.contains(superMethodWithArgs)) {
                        // 子类未被调用，不添加
                        continue;
                    }
                }

                childMethodAttributeMap.put(superMethodWithArgs, superMethodAttribute);

                // 添加子类调用父类方法
                String childCallSuperClassMethod = String.format(METHOD_CALL_FORMAT, callIdCounter.addAndGet(), childClassName, superMethodWithArgs,
                        CallTypeEnum.CTE_CCS.getType(), superClassName, superMethodWithArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
                writeResult(resultWriter, childCallSuperClassMethod + " " + JavaCGConstants.DEFAULT_JAR_NUM);
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
                if (!recordAll && calleeMethodWithArgsSet == null) {
                    // 接口未被调用，不添加
                    continue;
                }

                List<String> interfaceMethodWithArgsList = interfaceMethodWithArgsMap.get(interfaceName);
                if (interfaceMethodWithArgsList == null || interfaceMethodWithArgsList.isEmpty()) {
                    continue;
                }

                List<String> classMethodWithArgsList = classInterfaceMethodInfo.getMethodWithArgsList();
                for (String classMethodWithArgs : classMethodWithArgsList) {
                    if (!interfaceMethodWithArgsList.contains(classMethodWithArgs)) {
                        // 接口中不包含的方法，跳过
                        continue;
                    }

                    if (!recordAll && !calleeMethodWithArgsSet.contains(classMethodWithArgs)) {
                        // 方法未被调用，不添加
                        continue;
                    }

                    String interfaceCallClassMethod = String.format(METHOD_CALL_FORMAT, callIdCounter.addAndGet(), interfaceName, classMethodWithArgs,
                            CallTypeEnum.CTE_ITF.getType(), className, classMethodWithArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
                    writeResult(resultWriter, interfaceCallClassMethod + " " + JavaCGConstants.DEFAULT_JAR_NUM);
                }
            }
        }
    }

    // 对Class进行预处理
    private boolean preHandleClasses(String jarFilePath, File jarFile) {
        try (JarFile jar = new JarFile(jarFile)) {
            Enumeration<JarEntry> enumeration = jar.entries();
            while (enumeration.hasMoreElements()) {
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

            // 调用自定义接口实现类的方法
            for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
                customCodeParser.setClassInterfaceMethodInfoMap(classInterfaceMethodInfoMap);
            }

            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    // 对Class进行第二次预处理
    private boolean preHandleClasses2(String jarFilePath, File jarFile) {
        try (JarFile jar = new JarFile(jarFile)) {
            Enumeration<JarEntry> enumeration = jar.entries();
            while (enumeration.hasMoreElements()) {
                JarEntry jarEntry = enumeration.nextElement();
                if (!jarEntry.isDirectory() && jarEntry.getName().endsWith(".class")) {
                    ClassParser cp = new ClassParser(jarFilePath, jarEntry.getName());
                    JavaClass javaClass = cp.parse();

                    if (javaClass.isClass() && extendsClassesSet.contains(javaClass.getClassName())) {
                        // 查找涉及继承的类的信息，需要提前执行，使后续处理方法调用时，extendsClassMethodInfoMap的数据是完整的
                        findExtendsClassesInfo(javaClass);
                    }
                }
            }

            // 调用自定义接口实现类的方法
            for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
                customCodeParser.setExtendsClassMethodInfoMap(extendsClassMethodInfoMap);
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
                List<String> interfaceMethodWithArgsList = JavaCGUtil.genInterfaceAbstractMethodWithArgs(methods);
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

            List<String> implClassMethodWithArgsList = JavaCGUtil.genImplClassMethodWithArgs(methods);
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
            List<String> childrenClassInfoList = childrenClassInfoMap.computeIfAbsent(superClassName, k -> new ArrayList<>());
            childrenClassInfoList.add(className);
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

                    String methodWithArgs = methodName + JavaCGUtil.getArgListStr(method.getArgumentTypes());
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
        resultWriter.write(data + JavaCGConstants.NEW_LINE);
    }
}
