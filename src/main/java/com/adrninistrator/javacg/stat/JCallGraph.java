package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.conf.JavaCGConfManager;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.interfaces.InterfaceExtendsMethodInfo;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg.extensions.code_parser.CodeParserInterface;
import com.adrninistrator.javacg.extensions.code_parser.SpringXmlBeanParserInterface;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.handler.ExtendsImplHandler;
import com.adrninistrator.javacg.parser.JarEntryHandleParser;
import com.adrninistrator.javacg.parser.JarEntryPreHandle1Parser;
import com.adrninistrator.javacg.parser.JarEntryPreHandle2Parser;
import com.adrninistrator.javacg.spring.DefineSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGJarUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import com.adrninistrator.javacg.writer.WriterSupportSkip;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/8/21
 * @description: 入口类
 */
public class JCallGraph {
    private final JavaCGCounter callIdCounter = new JavaCGCounter(JavaCGConstants.METHOD_CALL_ID_START);
    private final JavaCGCounter classNumCounter = new JavaCGCounter(0);
    private final JavaCGCounter methodNumCounter = new JavaCGCounter(0);

    private final ExtensionsManager extensionsManager = new ExtensionsManager();

    private JarEntryPreHandle1Parser jarEntryPreHandle1Parser;
    private JarEntryPreHandle2Parser jarEntryPreHandle2Parser;
    private JarEntryHandleParser jarEntryHandleParser;

    private ExtendsImplHandler extendsImplHandler;

    /*
        保存需要处理的jar包文件名及对应的jar包信息
        key     jar包文件名
        value   jar包信息，包含jar包序号
     */
    private Map<String, JarInfo> jarInfoMap;

    private JavaCGConfInfo javaCGConfInfo;

    // java-callgraph2处理结果信息
    private JavaCGOutputInfo javaCGOutputInfo;

    private DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler;

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private Map<String, List<String>> duplicateClassNameMap = new HashMap<>();

    public static void main(String[] args) {
        new JCallGraph().run(new JavaCGConfigureWrapper(false));
    }

    public boolean run(JavaCGConfigureWrapper javaCGConfigureWrapper) {
        if (javaCGConfigureWrapper == null) {
            throw new JavaCGRuntimeException("配置参数包装对象不允许为null");
        }

        long startTime = System.currentTimeMillis();

        javaCGConfInfo = JavaCGConfManager.getConfInfo(javaCGConfigureWrapper);

        JavaCGLogUtil.setDebugPrintFlag(javaCGConfInfo.isDebugPrint());
        JavaCGLogUtil.setDebugPrintInFile(javaCGConfInfo.isDebugPrintInFile());

        // 处理参数中指定的jar包
        File newJarFile = handleJarInConf();
        if (newJarFile == null) {
            return false;
        }

        String newJarFilePath = JavaCGFileUtil.getCanonicalPath(newJarFile);
        String outputRootPath = javaCGConfInfo.getOutputRootPath();
        String outputDirPath;
        if (StringUtils.isBlank(outputRootPath)) {
            // 配置参数中未指定生成文件的根目录，生成在jar包所在目录
            outputDirPath = newJarFilePath + JavaCGConstants.DIR_TAIL_OUTPUT + File.separator;
        } else {
            // 配置参数中有指定生成文件的根目录，生成在指定目录
            outputDirPath = JavaCGUtil.addSeparator4FilePath(outputRootPath) + newJarFile.getName() + JavaCGConstants.DIR_TAIL_OUTPUT + File.separator;
        }
        System.out.println("当前输出的根目录: " + outputDirPath);
        if (!JavaCGFileUtil.isDirectoryExists(outputDirPath, true)) {
            return false;
        }

        // 初始化
        if (!init(outputDirPath)) {
            return false;
        }

        // 打印生成的文件信息
        printOutputFileInfo();

        try (Writer jarInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_JAR_INFO));
             Writer classNameWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_NAME));
             Writer methodCallWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL));
             Writer lambdaMethodInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_LAMBDA_METHOD_INFO));
             Writer classAnnotationWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_ANNOTATION));
             Writer methodAnnotationWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ANNOTATION));
             Writer methodLineNumberWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_LINE_NUMBER));
             Writer methodCallInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_INFO));
             Writer classInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_INFO));
             Writer methodInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_INFO));
             Writer extendsImplWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_EXTENDS_IMPL));
             Writer springBeanWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_SPRING_BEAN));
             Writer classSignatureEI1Writer = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_EI1));
             Writer methodArgGenericsTypeWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARG_GENERICS_TYPE));
             Writer methodReturnGenericsTypeWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_RETURN_GENERICS_TYPE));
             Writer innerClassWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_INNER_CLASS));
             WriterSupportSkip logMethodSpendTimeWriter = new WriterSupportSkip(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_LOG_METHOD_SPEND_TIME))
        ) {
            jarEntryHandleParser.setJarInfoWriter(jarInfoWriter);
            jarEntryHandleParser.setClassNameWriter(classNameWriter);
            jarEntryHandleParser.setMethodCallWriter(methodCallWriter);
            jarEntryHandleParser.setLambdaMethodInfoWriter(lambdaMethodInfoWriter);
            jarEntryHandleParser.setClassAnnotationWriter(classAnnotationWriter);
            jarEntryHandleParser.setMethodAnnotationWriter(methodAnnotationWriter);
            jarEntryHandleParser.setMethodLineNumberWriter(methodLineNumberWriter);
            jarEntryHandleParser.setMethodCallInfoWriter(methodCallInfoWriter);
            jarEntryHandleParser.setClassInfoWriter(classInfoWriter);
            jarEntryHandleParser.setMethodInfoWriter(methodInfoWriter);
            jarEntryHandleParser.setExtendsImplWriter(extendsImplWriter);
            jarEntryHandleParser.setClassSignatureEI1Writer(classSignatureEI1Writer);
            jarEntryHandleParser.setMethodArgGenericsTypeWriter(methodArgGenericsTypeWriter);
            jarEntryHandleParser.setMethodReturnGenericsTypeWriter(methodReturnGenericsTypeWriter);
            jarEntryHandleParser.setInnerClassWriter(innerClassWriter);
            jarEntryHandleParser.setLogMethodSpendTimeWriter(logMethodSpendTimeWriter);

            // 处理jar包
            if (!handleJar(newJarFilePath, methodCallWriter, springBeanWriter)) {
                return false;
            }

            long spendTime = System.currentTimeMillis() - startTime;
            String printInfo = "执行完毕，处理数量，类： " + classNumCounter.getCount() +
                    " ，方法: " + methodNumCounter.getCount() +
                    " ，方法调用: " + callIdCounter.getCount() +
                    " ，耗时: " + (spendTime / 1000.0D) + " S";
            System.out.println(printInfo);
            if (JavaCGLogUtil.isDebugPrintInFile()) {
                JavaCGLogUtil.debugPrint(printInfo);
            }
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            String errorInfo = "### 出现异常: " + e.getMessage();
            System.err.println(errorInfo);
            if (JavaCGLogUtil.isDebugPrintInFile()) {
                JavaCGLogUtil.debugPrint(errorInfo);
            }
            return false;
        } finally {
            // 关闭扩展类管理类
            extensionsManager.close();
        }
    }

    // 处理配置参数中指定的jar包
    private File handleJarInConf() {
        List<String> jarDirList = javaCGConfInfo.getJarDirList();
        if (jarDirList.isEmpty()) {
            System.err.println("请在配置文件" + JavaCGConstants.FILE_CONFIG + "中指定需要处理的jar包或目录列表");
            return null;
        }

        System.out.println("需要处理的jar包或目录:");
        for (String jarDir : jarDirList) {
            System.out.println(jarDir);
        }

        jarInfoMap = new HashMap<>(jarDirList.size());

        Set<String> needHandlePackageSet = javaCGConfInfo.getNeedHandlePackageSet();
        // 对指定的jar包进行处理
        File newJarFile = JavaCGJarUtil.handleJar(jarDirList, jarInfoMap, needHandlePackageSet);
        if (newJarFile == null) {
            return null;
        }

        System.out.println("实际处理的jar文件: " + JavaCGFileUtil.getCanonicalPath(newJarFile));

        if (needHandlePackageSet.isEmpty()) {
            System.out.println("所有包中的class文件都需要处理");
        } else {
            List<String> needHandlePackageList = new ArrayList<>(needHandlePackageSet);
            Collections.sort(needHandlePackageList);
            System.out.println("仅处理以下包中的class文件\n" + StringUtils.join(needHandlePackageList, "\n"));
        }
        return newJarFile;
    }

    private boolean init(String dirPath) {
        // 检查方法调用枚举类型是否重复定义
        JavaCGCallTypeEnum.checkRepeat();

        // 处理结果信息相关
        javaCGOutputInfo = new JavaCGOutputInfo(dirPath, javaCGConfInfo.getOutputFileExt());

        // 扩展类管理类初始化
        extensionsManager.setJavaCGOutputInfo(javaCGOutputInfo);
        if (!extensionsManager.init()) {
            return false;
        }

        // 第一次预处理相关
        // Runnable实现类Map
        Map<String, Boolean> runnableImplClassMap = new HashMap<>(JavaCGConstants.SIZE_100);
        // Callable实现类Map
        Map<String, Boolean> callableImplClassMap = new HashMap<>(JavaCGConstants.SIZE_100);
        // TransactionCallback实现类Map
        Map<String, Boolean> transactionCallbackImplClassMap = new HashMap<>(JavaCGConstants.SIZE_10);
        // TransactionCallbackWithoutResult子类Map
        Map<String, Boolean> transactionCallbackWithoutResultChildClassMap = new HashMap<>(JavaCGConstants.SIZE_10);
        // Thread子类Map
        Map<String, Boolean> threadChildClassMap = new HashMap<>(JavaCGConstants.SIZE_100);
        // 涉及继承的类名Set
        Set<String> classExtendsSet = new HashSet<>(JavaCGConstants.SIZE_100);
        // 涉及继承的接口名Set
        Set<String> interfaceExtendsSet = new HashSet<>(JavaCGConstants.SIZE_100);
  
        /*
           类名及所在的jar包序号Map
           key    类名
           value  类所在的jar包序号
        */
        ClassAndJarNum classAndJarNum = new ClassAndJarNum();

        /*
            类实现的接口，及类中的方法信息
            key     类名
            value   类实现的接口，及类中的方法信息
         */
        Map<String, ClassImplementsMethodInfo> classImplementsMethodInfoMap = new HashMap<>(JavaCGConstants.SIZE_200);

        /*
            接口中的方法信息
            key     接口名
            value   接口中的方法信息
        */
        Map<String, List<MethodArgReturnTypes>> interfaceMethodWithArgsMap = new HashMap<>(JavaCGConstants.SIZE_200);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            defineSpringBeanByAnnotationHandler = new DefineSpringBeanByAnnotationHandler(javaCGConfInfo);
        }
        jarEntryPreHandle1Parser = new JarEntryPreHandle1Parser(javaCGConfInfo, jarInfoMap, defineSpringBeanByAnnotationHandler, extensionsManager);
        jarEntryPreHandle1Parser.setClassImplementsMethodInfoMap(classImplementsMethodInfoMap);
        jarEntryPreHandle1Parser.setInterfaceMethodWithArgsMap(interfaceMethodWithArgsMap);
        jarEntryPreHandle1Parser.setRunnableImplClassMap(runnableImplClassMap);
        jarEntryPreHandle1Parser.setCallableImplClassMap(callableImplClassMap);
        jarEntryPreHandle1Parser.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        jarEntryPreHandle1Parser.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        jarEntryPreHandle1Parser.setThreadChildClassMap(threadChildClassMap);
        jarEntryPreHandle1Parser.setClassExtendsSet(classExtendsSet);
        jarEntryPreHandle1Parser.setInterfaceExtendsSet(interfaceExtendsSet);
        jarEntryPreHandle1Parser.setClassAndJarNum(classAndJarNum);

        // 第二次预处理相关
        /*
            类涉及继承的信息
            key
                类名
            value
                类涉及继承的信息，包含类的accessFlags，父类，及类中的方法信息
        */
        Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            父类对应的子类信息
            key
                父类类名
            value
                子类类名列表
         */
        Map<String, List<String>> childrenClassMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            接口涉及继承的信息
            key
                类名
            value
                接口继承的信息，包括接口继承的接口，及接口中的方法
        */
        Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            父接口对应的子接口信息
            key
                父接口类名
            value
                子接口类名列表
         */
        Map<String, List<String>> childrenInterfaceMap = new HashMap<>(JavaCGConstants.SIZE_100);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            useSpringBeanByAnnotationHandler = new UseSpringBeanByAnnotationHandler(
                    classExtendsMethodInfoMap,
                    classImplementsMethodInfoMap,
                    interfaceExtendsMethodInfoMap,
                    defineSpringBeanByAnnotationHandler,
                    extensionsManager.getSpringXmlBeanParser());
        }
        jarEntryPreHandle2Parser = new JarEntryPreHandle2Parser(javaCGConfInfo, jarInfoMap, useSpringBeanByAnnotationHandler);
        jarEntryPreHandle2Parser.setClassExtendsSet(classExtendsSet);
        jarEntryPreHandle2Parser.setClassExtendsMethodInfoMap(classExtendsMethodInfoMap);
        jarEntryPreHandle2Parser.setChildrenClassMap(childrenClassMap);
        jarEntryPreHandle2Parser.setInterfaceExtendsSet(interfaceExtendsSet);
        jarEntryPreHandle2Parser.setInterfaceExtendsMethodInfoMap(interfaceExtendsMethodInfoMap);
        jarEntryPreHandle2Parser.setChildrenInterfaceMap(childrenInterfaceMap);

        // 正式处理相关
        jarEntryHandleParser = new JarEntryHandleParser(javaCGConfInfo, jarInfoMap);
        jarEntryHandleParser.setUseSpringBeanByAnnotationHandler(useSpringBeanByAnnotationHandler);
        jarEntryHandleParser.setRunnableImplClassMap(runnableImplClassMap);
        jarEntryHandleParser.setCallableImplClassMap(callableImplClassMap);
        jarEntryHandleParser.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        jarEntryHandleParser.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        jarEntryHandleParser.setThreadChildClassMap(threadChildClassMap);
        jarEntryHandleParser.setExtensionsManager(extensionsManager);
        jarEntryHandleParser.setCallIdCounter(callIdCounter);
        jarEntryHandleParser.setClassNumCounter(classNumCounter);
        jarEntryHandleParser.setMethodNumCounter(methodNumCounter);
        jarEntryHandleParser.setClassAndJarNum(classAndJarNum);

        // 继承及实现相关的方法处理相关
        extendsImplHandler = new ExtendsImplHandler();
        extendsImplHandler.setJavaCGConfInfo(javaCGConfInfo);
        extendsImplHandler.setCallIdCounter(callIdCounter);
        extendsImplHandler.setInterfaceMethodWithArgsMap(interfaceMethodWithArgsMap);
        extendsImplHandler.setChildrenClassMap(childrenClassMap);
        extendsImplHandler.setInterfaceExtendsMethodInfoMap(interfaceExtendsMethodInfoMap);
        extendsImplHandler.setChildrenInterfaceMap(childrenInterfaceMap);
        extendsImplHandler.setClassImplementsMethodInfoMap(classImplementsMethodInfoMap);
        extendsImplHandler.setClassExtendsMethodInfoMap(classExtendsMethodInfoMap);
        extendsImplHandler.setClassAndJarNum(classAndJarNum);
        return true;
    }

    // 打印生成的文件信息
    private void printOutputFileInfo() {
        System.out.println("写入文件:");
        for (JavaCGOutPutFileTypeEnum javaCGOutPutFileTypeEnum : JavaCGOutPutFileTypeEnum.values()) {
            System.out.println(javaCGOutputInfo.getMainFilePath(javaCGOutPutFileTypeEnum));
        }

        Set<String> otherFileNameSet = javaCGOutputInfo.getOtherFileNameSet();
        if (otherFileNameSet.isEmpty()) {
            System.out.println("不写入其他文件");
            return;
        }

        System.out.println("写入其他文件");
        for (String otherFileName : otherFileNameSet) {
            System.out.println(otherFileName + " " + javaCGOutputInfo.getOtherFilePath(otherFileName));
        }
    }

    // 处理一个jar包
    private boolean handleJar(String jarFilePath, Writer methodCallWriter, Writer springBeanWriter) {
        try {
            // 对Class进行预处理
            if (!preHandleClasses1(jarFilePath)) {
                return false;
            }

            // 对Class进行第二次预处理
            if (!preHandleClasses2(jarFilePath)) {
                return false;
            }

            // 处理当前jar包中的class文件
            if (!jarEntryHandleParser.parse(jarFilePath)) {
                return false;
            }

            duplicateClassNameMap = jarEntryHandleParser.getDuplicateClassNameMap();
            // 打印重复的类名
            printDuplicateClasses();

            // 处理继承及实现相关的方法
            extendsImplHandler.handle(methodCallWriter);

            // 记录Spring Bean的名称及类型
            recordSpringBeanNameAndType(springBeanWriter);
            return true;
        } catch (Exception e) {
            System.err.println("处理jar包出现异常 " + jarFilePath);
            e.printStackTrace();
            return false;
        }
    }

    // 对Class进行预处理
    private boolean preHandleClasses1(String jarFilePath) {
        return jarEntryPreHandle1Parser.parse(jarFilePath);
    }

    // 对Class进行第二次预处理
    private boolean preHandleClasses2(String jarFilePath) {
        return jarEntryPreHandle2Parser.parse(jarFilePath);
    }

    // 记录Spring Bean的名称及类型
    private void recordSpringBeanNameAndType(Writer springBeanWriter) throws IOException {
        if (defineSpringBeanByAnnotationHandler == null) {
            return;
        }

        for (String springBeanName : defineSpringBeanByAnnotationHandler.getSpringBeanNameSet()) {
            List<String> springBeanTypeList = defineSpringBeanByAnnotationHandler.getSpringBeanTypeList(springBeanName);
            for (int i = 0; i < springBeanTypeList.size(); i++) {
                JavaCGFileUtil.write2FileWithTab(springBeanWriter, springBeanName, String.valueOf(i), springBeanTypeList.get(i));
            }
        }
    }

    // 打印重复的类名
    private void printDuplicateClasses() {
        if (duplicateClassNameMap.isEmpty()) {
            JavaCGLogUtil.debugPrint("不存在重复的类名");
            return;
        }

        List<String> duplicateClassNameList = new ArrayList<>(duplicateClassNameMap.keySet());
        Collections.sort(duplicateClassNameList);

        for (String duplicateClassName : duplicateClassNameList) {
            List<String> classFilePathList = duplicateClassNameMap.get(duplicateClassName);
            JavaCGLogUtil.debugPrint("重复的类名 " + duplicateClassName + " 使用的class文件 " + classFilePathList.get(0));
            for (int i = 1; i < classFilePathList.size(); i++) {
                JavaCGLogUtil.debugPrint("重复的类名 " + duplicateClassName + " 跳过的class文件 " + classFilePathList.get(i));
            }
        }
    }

    /**
     * 添加自定义代码解析类
     * 需要在调用run()方法之前调用当前方法
     *
     * @param codeParser
     */
    public void addCodeParser(CodeParserInterface codeParser) {
        extensionsManager.addCodeParser(codeParser);
    }

    /**
     * 设置对Spring XML中的Bean解析的类
     * 需要在调用run()方法之前调用当前方法
     *
     * @param springXmlBeanParser
     */
    public void setSpringXmlBeanParser(SpringXmlBeanParserInterface springXmlBeanParser) {
        extensionsManager.setSpringXmlBeanParser(springXmlBeanParser);
    }

    // 获取java-callgraph2处理结果信息
    public JavaCGOutputInfo getJavaCGOutputInfo() {
        return javaCGOutputInfo;
    }

    /**
     * 获取重复类名Map
     *
     * @return
     */
    public Map<String, List<String>> getDuplicateClassNameMap() {
        return duplicateClassNameMap;
    }

    /**
     * 设置注解属性格式化类
     *
     * @param annotationAttributesFormatter
     */
    public void setAnnotationAttributesFormatter(AnnotationAttributesFormatterInterface annotationAttributesFormatter) {
        extensionsManager.setAnnotationAttributesFormatter(annotationAttributesFormatter);
    }
}
