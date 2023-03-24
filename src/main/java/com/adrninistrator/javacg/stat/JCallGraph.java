package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.conf.JavaCGConfManager;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.interfaces.InterfaceExtendsMethodInfo;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.dto.method.MethodAndArgs;
import com.adrninistrator.javacg.dto.output.HandleOutputInfo;
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

    // 保存需要处理的jar包文件名及对应的序号
    private Map<String, JarInfo> jarInfoMap;

    private JavaCGConfInfo javaCGConfInfo;

    // java-callgraph2处理结果信息
    private HandleOutputInfo handleOutputInfo;

    private DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler;

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private Map<String, List<String>> duplicateClassNameMap = new HashMap<>();

    public static void main(String[] args) {
        new JCallGraph().run(new JavaCGConfigureWrapper());
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
        String newJarFilePath = handleJarInArgs();
        if (newJarFilePath == null) {
            return false;
        }

        String dirPath = newJarFilePath + JavaCGConstants.DIR_TAIL_OUTPUT + File.separator;
        if (!JavaCGFileUtil.isDirectoryExists(dirPath, true)) {
            return false;
        }

        // 初始化
        if (!init(dirPath)) {
            return false;
        }

        // 打印生成的文件信息
        printOutputFileInfo();

        try (Writer jarInfoWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getJarInfoOutputFilePath());
             Writer classNameWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getClassNameOutputFilePath());
             Writer methodCallWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getMethodCallOutputFilePath());
             Writer lambdaMethodInfoWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getLambdaMethodInfoOutputFilePath());
             Writer classAnnotationWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getClassAnnotationOutputFilePath());
             Writer methodAnnotationWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getMethodAnnotationOutputFilePath());
             Writer methodLineNumberWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getMethodLineNumberOutputFilePath());
             Writer methodCallInfoWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getMethodCallInfoOutputFilePath());
             Writer classInfoWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getClassInfoOutputFilePath());
             Writer methodInfoWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getMethodInfoOutputFilePath());
             Writer extendsImplWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getExtendsImplOutputFilePath());
             Writer springBeanWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getSpringBeanOutputFilePath());
             Writer classSignatureEI1Writer = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getClassSignatureEI1OutputFilePath());
             Writer methodArgGenericsTypeWriter = JavaCGFileUtil.genBufferedWriter(handleOutputInfo.getMethodArgGenericsTypeFilePath())
        ){
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
        } catch(Exception e){
            e.printStackTrace();
            String errorInfo = "### 出现异常: " + e.getMessage();
            System.err.println(errorInfo);
            if (JavaCGLogUtil.isDebugPrintInFile()) {
                JavaCGLogUtil.debugPrint(errorInfo);
            }
            return false;
        } finally{
            // 关闭扩展类管理类
            if (extensionsManager != null) {
                extensionsManager.close();
            }
        }
    }

    // 处理参数中指定的jar包
    private String handleJarInArgs() {
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

        // 对指定的jar包进行处理
        String newJarFilePath = JavaCGJarUtil.handleJar(jarDirList, jarInfoMap, javaCGConfInfo.getNeedHandlePackageSet());
        if (newJarFilePath == null) {
            return null;
        }

        System.out.println("实际处理的jar文件: " + newJarFilePath);

        if (javaCGConfInfo.getNeedHandlePackageSet().isEmpty()) {
            System.out.println("所有包中的class文件都需要处理");
        } else {
            System.out.println("仅处理以下包中的class文件\n" + StringUtils.join(javaCGConfInfo.getNeedHandlePackageSet(), "\n"));
        }
        return newJarFilePath;
    }

    private boolean init(String dirPath) {
        // 检查方法调用枚举类型是否重复定义
        JavaCGCallTypeEnum.checkRepeat();

        // 处理结果信息相关
        handleOutputInfo = new HandleOutputInfo(dirPath, javaCGConfInfo.getOutputFileExt());

        // 扩展类管理类初始化
        extensionsManager.setHandleOutputInfo(handleOutputInfo);
        if (!extensionsManager.init()) {
            return false;
        }

        // 第一次预处理相关
        Map<String, Boolean> runnableImplClassMap = new HashMap<>(JavaCGConstants.SIZE_100);
        Map<String, Boolean> callableImplClassMap = new HashMap<>(JavaCGConstants.SIZE_100);
        Map<String, Boolean> transactionCallbackImplClassMap = new HashMap<>(JavaCGConstants.SIZE_10);
        Map<String, Boolean> transactionCallbackWithoutResultChildClassMap = new HashMap<>(JavaCGConstants.SIZE_10);
        Map<String, Boolean> threadChildClassMap = new HashMap<>(JavaCGConstants.SIZE_100);
        Set<String> classExtendsSet = new HashSet<>(JavaCGConstants.SIZE_100);
        Set<String> interfaceExtendsSet = new HashSet<>(JavaCGConstants.SIZE_100);

        /*
            类实现的接口，及类中的方法信息
            key
                类名
            value
                类实现的接口，及类中的方法信息
         */
        Map<String, ClassImplementsMethodInfo> classInterfaceMethodInfoMap = new HashMap<>(JavaCGConstants.SIZE_200);

        /*
            接口中的方法信息
            key
                接口名
            value
                接口中的方法信息
        */
        Map<String, List<MethodAndArgs>> interfaceMethodWithArgsMap = new HashMap<>(JavaCGConstants.SIZE_200);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            defineSpringBeanByAnnotationHandler = new DefineSpringBeanByAnnotationHandler(javaCGConfInfo);
        }
        jarEntryPreHandle1Parser = new JarEntryPreHandle1Parser(javaCGConfInfo, defineSpringBeanByAnnotationHandler, extensionsManager);
        jarEntryPreHandle1Parser.setClassInterfaceMethodInfoMap(classInterfaceMethodInfoMap);
        jarEntryPreHandle1Parser.setInterfaceMethodWithArgsMap(interfaceMethodWithArgsMap);
        jarEntryPreHandle1Parser.setRunnableImplClassMap(runnableImplClassMap);
        jarEntryPreHandle1Parser.setCallableImplClassMap(callableImplClassMap);
        jarEntryPreHandle1Parser.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        jarEntryPreHandle1Parser.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        jarEntryPreHandle1Parser.setThreadChildClassMap(threadChildClassMap);
        jarEntryPreHandle1Parser.setClassExtendsSet(classExtendsSet);
        jarEntryPreHandle1Parser.setInterfaceExtendsSet(interfaceExtendsSet);

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
            useSpringBeanByAnnotationHandler = new UseSpringBeanByAnnotationHandler(classExtendsMethodInfoMap, defineSpringBeanByAnnotationHandler,
                    extensionsManager.getSpringXmlBeanParser());
        }
        jarEntryPreHandle2Parser = new JarEntryPreHandle2Parser(javaCGConfInfo, useSpringBeanByAnnotationHandler);
        jarEntryPreHandle2Parser.setClassExtendsSet(classExtendsSet);
        jarEntryPreHandle2Parser.setClassExtendsMethodInfoMap(classExtendsMethodInfoMap);
        jarEntryPreHandle2Parser.setChildrenClassMap(childrenClassMap);
        jarEntryPreHandle2Parser.setInterfaceExtendsSet(interfaceExtendsSet);
        jarEntryPreHandle2Parser.setInterfaceExtendsMethodInfoMap(interfaceExtendsMethodInfoMap);
        jarEntryPreHandle2Parser.setChildrenInterfaceMap(childrenInterfaceMap);

        // 正式处理相关
        jarEntryHandleParser = new JarEntryHandleParser(javaCGConfInfo);
        jarEntryHandleParser.setUseSpringBeanByAnnotationHandler(useSpringBeanByAnnotationHandler);
        jarEntryHandleParser.setRunnableImplClassMap(runnableImplClassMap);
        jarEntryHandleParser.setCallableImplClassMap(callableImplClassMap);
        jarEntryHandleParser.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        jarEntryHandleParser.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        jarEntryHandleParser.setThreadChildClassMap(threadChildClassMap);
        jarEntryHandleParser.setJarInfoMap(jarInfoMap);
        jarEntryHandleParser.setExtensionsManager(extensionsManager);
        jarEntryHandleParser.setCallIdCounter(callIdCounter);
        jarEntryHandleParser.setClassNumCounter(classNumCounter);
        jarEntryHandleParser.setMethodNumCounter(methodNumCounter);

        // 继承及实现相关的方法处理相关
        extendsImplHandler = new ExtendsImplHandler();
        extendsImplHandler.setJavaCGConfInfo(javaCGConfInfo);
        extendsImplHandler.setCallIdCounter(callIdCounter);
        extendsImplHandler.setInterfaceMethodWithArgsMap(interfaceMethodWithArgsMap);
        extendsImplHandler.setChildrenClassMap(childrenClassMap);
        extendsImplHandler.setInterfaceExtendsMethodInfoMap(interfaceExtendsMethodInfoMap);
        extendsImplHandler.setChildrenInterfaceMap(childrenInterfaceMap);
        extendsImplHandler.setClassInterfaceMethodInfoMap(classInterfaceMethodInfoMap);
        extendsImplHandler.setClassExtendsMethodInfoMap(classExtendsMethodInfoMap);
        return true;
    }

    // 打印生成的文件信息
    private void printOutputFileInfo() {
        System.out.println("写入文件:" +
                "\n" + handleOutputInfo.getJarInfoOutputFilePath() +
                "\n" + handleOutputInfo.getClassNameOutputFilePath() +
                "\n" + handleOutputInfo.getMethodCallOutputFilePath() +
                "\n" + handleOutputInfo.getLambdaMethodInfoOutputFilePath() +
                "\n" + handleOutputInfo.getClassAnnotationOutputFilePath() +
                "\n" + handleOutputInfo.getMethodAnnotationOutputFilePath() +
                "\n" + handleOutputInfo.getMethodLineNumberOutputFilePath() +
                "\n" + handleOutputInfo.getMethodCallInfoOutputFilePath() +
                "\n" + handleOutputInfo.getClassInfoOutputFilePath() +
                "\n" + handleOutputInfo.getMethodInfoOutputFilePath() +
                "\n" + handleOutputInfo.getExtendsImplOutputFilePath() +
                "\n" + handleOutputInfo.getSpringBeanOutputFilePath() +
                "\n" + handleOutputInfo.getClassSignatureEI1OutputFilePath() +
                "\n" + handleOutputInfo.getMethodArgGenericsTypeFilePath()
        );

        Set<String> otherFileNameSet = handleOutputInfo.getOtherFileNameSet();
        if (otherFileNameSet.isEmpty()) {
            System.out.println("不写入其他文件");
            return;
        }

        System.out.println("写入其他文件");
        for (String otherFileName : otherFileNameSet) {
            System.out.println(otherFileName + " " + handleOutputInfo.getOtherFilePath(otherFileName));
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
    public HandleOutputInfo getHandleOutputInfo() {
        return handleOutputInfo;
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
