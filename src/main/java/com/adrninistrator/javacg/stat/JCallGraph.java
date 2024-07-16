package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.conf.JavaCGConfManager;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.dto.classes.ClassExtendsInfo;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.extensions.annotationattributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg.extensions.codeparser.CodeParserInterface;
import com.adrninistrator.javacg.extensions.codeparser.SpringXmlBeanParserInterface;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.handler.ExtendsImplHandler;
import com.adrninistrator.javacg.parser.JarEntryHandleParser;
import com.adrninistrator.javacg.parser.JarEntryPreHandle1Parser;
import com.adrninistrator.javacg.parser.JarEntryPreHandle2Parser;
import com.adrninistrator.javacg.spring.DefineSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGJarUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import com.adrninistrator.javacg.writer.WriterSupportSkip;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    private static final Logger logger = LoggerFactory.getLogger(JCallGraph.class);

    private final JavaCGCounter callIdCounter = new JavaCGCounter(JavaCGConstants.METHOD_CALL_ID_MIN_BEFORE);
    private final JavaCGCounter classNumCounter = new JavaCGCounter();
    private final JavaCGCounter methodNumCounter = new JavaCGCounter();
    private final JavaCGCounter failCounter = new JavaCGCounter();
    private final JavaCGCounter fieldRelationshipCounter = new JavaCGCounter(JavaCGConstants.RECORD_ID_MIN_BEFORE);

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
        if (javaCGConfInfo == null) {
            return false;
        }

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
        // 记录当前使用的生成文件的目录
        javaCGConfInfo.setUsedOutputDirPath(outputDirPath);
        logger.info("当前输出的根目录: {}", outputDirPath);
        if (!JavaCGFileUtil.isDirectoryExists(outputDirPath, true)) {
            return false;
        }

        // 初始化
        if (!init(javaCGConfigureWrapper, outputDirPath)) {
            return false;
        }

        // 打印生成的文件信息
        printOutputFileInfo();

        try (Writer classAnnotationWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_ANNOTATION));
             Writer classInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_INFO));
             Writer classNameWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_NAME));
             Writer classSignatureEI1Writer = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_EI1));
             Writer classSignatureGenericsWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_GENERICS));
             Writer classSigExtImplGenericsWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIG_EXT_IMPL_GENERICS));
             Writer extendsImplWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_EXTENDS_IMPL));
             Writer fieldAnnotationWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_FIELD_ANNOTATION));
             Writer fieldInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_FIELD_INFO));
             Writer fieldGenericsTypeWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_FIELD_GENERICS_TYPE));
             Writer fieldRelationshipWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_FIELD_RELATIONSHIP));
             Writer getMethodWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_GET_METHOD));
             Writer innerClassWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_INNER_CLASS));
             Writer jarInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_JAR_INFO));
             Writer lambdaMethodInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_LAMBDA_METHOD_INFO));
             Writer methodAnnotationWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ANNOTATION));
             Writer methodArgumentWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARGUMENT));
             Writer methodArgAnnotationWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARG_ANNOTATION));
             Writer methodArgGenericsTypeWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARG_GENERICS_TYPE));
             Writer methodCallInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_INFO));
             Writer methodCallMethodCallReturnWriter =
                     JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_METHOD_CALL_RETURN));
             Writer methodCallStaticFieldWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL_STATIC_FIELD));
             Writer methodReturnArgSeqWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_RETURN_ARG_SEQ));
             Writer methodReturnCallIdWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_RETURN_CALL_ID));
             Writer methodCallWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL));
             Writer methodInfoWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_INFO));
             Writer methodLineNumberWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_LINE_NUMBER));
             Writer methodReturnGenericsTypeWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_RETURN_GENERICS_TYPE));
             Writer methodCatchWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CATCH));
             Writer methodFinallyWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_FINALLY));
             Writer methodThrowWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_THROW));
             Writer setMethodWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_SET_METHOD));
             Writer springBeanWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_SPRING_BEAN));
             Writer staticFinalFieldMethodCallIdWriter = JavaCGFileUtil.genBufferedWriter(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_SF_FIELD_METHOD_CALL));
             WriterSupportSkip logMethodSpendTimeWriter = new WriterSupportSkip(javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_LOG_METHOD_SPEND_TIME))
        ) {
            JavaCGFileUtil.write2FileWithTab(jarInfoWriter, JavaCGConstants.FILE_KEY_RESULT_DIR_INFO_PREFIX, String.valueOf(JavaCGConstants.RECORD_ID_MIN),
                    javaCGOutputInfo.getOutputDirPath());

            jarEntryHandleParser.setClassAnnotationWriter(classAnnotationWriter);
            jarEntryHandleParser.setClassInfoWriter(classInfoWriter);
            jarEntryHandleParser.setClassNameWriter(classNameWriter);
            jarEntryHandleParser.setClassSignatureEI1Writer(classSignatureEI1Writer);
            jarEntryHandleParser.setClassSignatureGenericsWriter(classSignatureGenericsWriter);
            jarEntryHandleParser.setClassSigExtImplGenericsWriter(classSigExtImplGenericsWriter);
            jarEntryHandleParser.setExtendsImplWriter(extendsImplWriter);
            jarEntryHandleParser.setFieldAnnotationWriter(fieldAnnotationWriter);
            jarEntryHandleParser.setFieldInfoWriter(fieldInfoWriter);
            jarEntryHandleParser.setFieldGenericsTypeWriter(fieldGenericsTypeWriter);
            jarEntryHandleParser.setFieldRelationshipWriter(fieldRelationshipWriter);
            jarEntryHandleParser.setGetMethodWriter(getMethodWriter);
            jarEntryHandleParser.setInnerClassWriter(innerClassWriter);
            jarEntryHandleParser.setJarInfoWriter(jarInfoWriter);
            jarEntryHandleParser.setLambdaMethodInfoWriter(lambdaMethodInfoWriter);
            jarEntryHandleParser.setMethodAnnotationWriter(methodAnnotationWriter);
            jarEntryHandleParser.setMethodArgumentWriter(methodArgumentWriter);
            jarEntryHandleParser.setMethodArgAnnotationWriter(methodArgAnnotationWriter);
            jarEntryHandleParser.setMethodArgGenericsTypeWriter(methodArgGenericsTypeWriter);
            jarEntryHandleParser.setMethodCallInfoWriter(methodCallInfoWriter);
            jarEntryHandleParser.setMethodCallMethodCallReturnWriter(methodCallMethodCallReturnWriter);
            jarEntryHandleParser.setMethodCallStaticFieldWriter(methodCallStaticFieldWriter);
            jarEntryHandleParser.setMethodReturnArgSeqWriter(methodReturnArgSeqWriter);
            jarEntryHandleParser.setMethodReturnCallIdWriter(methodReturnCallIdWriter);
            jarEntryHandleParser.setMethodCallWriter(methodCallWriter);
            jarEntryHandleParser.setMethodInfoWriter(methodInfoWriter);
            jarEntryHandleParser.setMethodLineNumberWriter(methodLineNumberWriter);
            jarEntryHandleParser.setMethodReturnGenericsTypeWriter(methodReturnGenericsTypeWriter);
            jarEntryHandleParser.setMethodCatchWriter(methodCatchWriter);
            jarEntryHandleParser.setMethodFinallyWriter(methodFinallyWriter);
            jarEntryHandleParser.setMethodThrowWriter(methodThrowWriter);
            jarEntryHandleParser.setSetMethodWriter(setMethodWriter);
            jarEntryHandleParser.setStaticFinalFieldMethodCallIdWriter(staticFinalFieldMethodCallIdWriter);

            jarEntryHandleParser.setLogMethodSpendTimeWriter(logMethodSpendTimeWriter);

            // 处理jar包
            if (!handleJar(newJarFilePath, methodCallWriter, springBeanWriter) || failCounter.getCount() > 0) {
                logger.error("处理失败，出错次数 {}", failCounter.getCount());
                return false;
            }

            long spendTime = System.currentTimeMillis() - startTime;
            logger.info("执行完毕，处理数量，类： {} 方法: {} 方法调用: {} 耗时: {} 秒", classNumCounter.getCount(), methodNumCounter.getCount(), callIdCounter.getCount(), spendTime / 1000.0D);
            return true;
        } catch (Exception e) {
            logger.error("出现异常 ", e);
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
            logger.error("请在配置文件 {} 中指定需要处理的jar包或目录列表", JavaCGOtherConfigFileUseListEnum.OCFULE_JAR_DIR.getFileName());
            return null;
        }

        logger.info("需要处理的jar包或目录:\n{}", StringUtils.join(jarDirList, "\n"));

        jarInfoMap = new HashMap<>(jarDirList.size());

        Set<String> needHandlePackageSet = javaCGConfInfo.getNeedHandlePackageSet();
        Set<String> jarDirMergeFileTypeSet = javaCGConfInfo.getJarDirMergeFileTypeSet();
        // 对指定的jar包进行处理
        File newJarFile = JavaCGJarUtil.handleJar(jarDirList, jarInfoMap, needHandlePackageSet, jarDirMergeFileTypeSet);
        if (newJarFile == null) {
            return null;
        }

        logger.info("实际处理的jar文件: {}", JavaCGFileUtil.getCanonicalPath(newJarFile));

        if (needHandlePackageSet.isEmpty()) {
            logger.info("所有包中的class文件都需要处理");
        } else {
            List<String> needHandlePackageList = new ArrayList<>(needHandlePackageSet);
            Collections.sort(needHandlePackageList);
            logger.info("仅处理以下包中的class文件\n{}", StringUtils.join(needHandlePackageList, "\n"));
        }
        return newJarFile;
    }

    private boolean init(JavaCGConfigureWrapper javaCGConfigureWrapper, String outputDirPath) {
        // 检查方法调用枚举类型是否重复定义
        JavaCGCallTypeEnum.checkRepeat();

        // 处理结果信息相关
        javaCGOutputInfo = new JavaCGOutputInfo(outputDirPath, javaCGConfInfo.getOutputFileExt());

        // 扩展类管理类初始化
        extensionsManager.setJavaCGOutputInfo(javaCGOutputInfo);
        if (!extensionsManager.init()) {
            return false;
        }

        // 检查对jar包中的其他文件解析时需要处理的文件类型，是否有在合并jar包或目录时需要合并的文件类型中指定
        if (!checkJarDirMergeFileType(javaCGConfigureWrapper)) {
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
        // 类名及对应的父类（父类非Object时记录）
        Map<String, String> classAndSuperMap = new HashMap<>(JavaCGConstants.SIZE_100);
        // 涉及继承的接口名Set
        Set<String> interfaceExtendsSet = new HashSet<>(JavaCGConstants.SIZE_100);
        // 所有在jar包中出现的类名Set
        Set<String> allClassNameSet = new HashSet<>(JavaCGConstants.SIZE_1000);
  
        /*
           类名及所在的jar包序号Map
           key    类名
           value  类所在的jar包序号
        */
        ClassAndJarNum classAndJarNum = new ClassAndJarNum();

        /*
            类实现的接口，及类中的方法信息
            key     类名
            value   类实现的接口
         */
        Map<String, List<String>> classImplementsInfoMap = new HashMap<>(JavaCGConstants.SIZE_200);

        /*
            涉及继承（父类非Object）、实现的类中，可能涉及继承（父类与子类）、实现（实现类）的方法
            key     类名
            value   相关方法
                key     方法名称、参数、返回值
                value   方法access_flags
         */
        Map<String, Map<MethodArgReturnTypes, Integer>> classExtendsImplMethodWithArgTypesMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            接口中的方法信息
            key     接口名
            value   接口中的方法信息
        */
        Map<String, Map<MethodArgReturnTypes, Integer>> interfaceMethodWithArgTypesMap = new HashMap<>(JavaCGConstants.SIZE_200);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            defineSpringBeanByAnnotationHandler = new DefineSpringBeanByAnnotationHandler(javaCGConfInfo, failCounter);
        }
        jarEntryPreHandle1Parser = new JarEntryPreHandle1Parser(javaCGConfInfo, jarInfoMap, defineSpringBeanByAnnotationHandler, extensionsManager);
        jarEntryPreHandle1Parser.setClassImplementsInfoMap(classImplementsInfoMap);
        jarEntryPreHandle1Parser.setClassExtendsImplMethodWithArgTypesMap(classExtendsImplMethodWithArgTypesMap);
        jarEntryPreHandle1Parser.setInterfaceMethodWithArgTypesMap(interfaceMethodWithArgTypesMap);
        jarEntryPreHandle1Parser.setRunnableImplClassMap(runnableImplClassMap);
        jarEntryPreHandle1Parser.setCallableImplClassMap(callableImplClassMap);
        jarEntryPreHandle1Parser.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        jarEntryPreHandle1Parser.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        jarEntryPreHandle1Parser.setThreadChildClassMap(threadChildClassMap);
        jarEntryPreHandle1Parser.setClassAndSuperMap(classAndSuperMap);
        jarEntryPreHandle1Parser.setInterfaceExtendsSet(interfaceExtendsSet);
        jarEntryPreHandle1Parser.setAllClassNameSet(allClassNameSet);
        jarEntryPreHandle1Parser.setClassAndJarNum(classAndJarNum);

        // 第二次预处理相关
        /*
            类涉及继承的信息
            key     类名
            value   类涉及继承的信息，包含类的accessFlags，父类
        */
        Map<String, ClassExtendsInfo> classExtendsInfoMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            父类对应的子类信息
            key     父类类名
            value   子类类名列表
         */
        Map<String, List<String>> childrenClassMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            接口涉及继承的信息
            key     类名
            value   接口继承的信息
        */
        Map<String, List<String>> interfaceExtendsInfoMap = new HashMap<>(JavaCGConstants.SIZE_100);

        /*
            父接口对应的子接口信息
            key     父接口类名
            value   子接口类名列表
         */
        Map<String, List<String>> childrenInterfaceMap = new HashMap<>(JavaCGConstants.SIZE_100);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            useSpringBeanByAnnotationHandler = new UseSpringBeanByAnnotationHandler(
                    classExtendsInfoMap,
                    classImplementsInfoMap,
                    interfaceExtendsInfoMap,
                    defineSpringBeanByAnnotationHandler,
                    extensionsManager.getSpringXmlBeanParser());
        }
        jarEntryPreHandle2Parser = new JarEntryPreHandle2Parser(javaCGConfInfo, jarInfoMap, useSpringBeanByAnnotationHandler);
        jarEntryPreHandle2Parser.setClassExtendsImplMethodWithArgTypesMap(classExtendsImplMethodWithArgTypesMap);
        jarEntryPreHandle2Parser.setInterfaceMethodWithArgTypesMap(interfaceMethodWithArgTypesMap);
        jarEntryPreHandle2Parser.setClassAndSuperMap(classAndSuperMap);
        jarEntryPreHandle2Parser.setClassExtendsInfoMap(classExtendsInfoMap);
        jarEntryPreHandle2Parser.setChildrenClassMap(childrenClassMap);
        jarEntryPreHandle2Parser.setInterfaceExtendsSet(interfaceExtendsSet);
        jarEntryPreHandle2Parser.setAllClassNameSet(allClassNameSet);
        jarEntryPreHandle2Parser.setInterfaceExtendsInfoMap(interfaceExtendsInfoMap);
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
        jarEntryHandleParser.setFailCounter(failCounter);
        jarEntryHandleParser.setFieldRelationshipCounter(fieldRelationshipCounter);
        jarEntryHandleParser.setClassAndJarNum(classAndJarNum);

        // 继承及实现相关的方法处理相关
        extendsImplHandler = new ExtendsImplHandler();
        extendsImplHandler.setJavaCGConfInfo(javaCGConfInfo);
        extendsImplHandler.setCallIdCounter(callIdCounter);
        extendsImplHandler.setInterfaceMethodWithArgTypesMap(interfaceMethodWithArgTypesMap);
        extendsImplHandler.setClassExtendsImplMethodWithArgTypesMap(classExtendsImplMethodWithArgTypesMap);
        extendsImplHandler.setChildrenClassMap(childrenClassMap);
        extendsImplHandler.setInterfaceExtendsInfoMap(interfaceExtendsInfoMap);
        extendsImplHandler.setChildrenInterfaceMap(childrenInterfaceMap);
        extendsImplHandler.setClassImplementsInfoMap(classImplementsInfoMap);
        extendsImplHandler.setClassExtendsInfoMap(classExtendsInfoMap);
        extendsImplHandler.setAllClassNameSet(allClassNameSet);
        extendsImplHandler.setClassAndJarNum(classAndJarNum);
        return true;
    }

    /**
     * 检查对jar包中的其他文件解析时需要处理的文件类型，是否有在合并jar包或目录时需要合并的文件类型中指定
     *
     * @param javaCGConfigureWrapper
     * @return
     */
    private boolean checkJarDirMergeFileType(JavaCGConfigureWrapper javaCGConfigureWrapper) {
        Set<String> jarDirMergeFileTypeSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_JAR_DIR_MERGE_FILE_TYPE, false);
        Set<String> lowerJarDirMergeFileTypeSet = new HashSet<>(jarDirMergeFileTypeSet.size());
        for (String jarDirMergeFileType : jarDirMergeFileTypeSet) {
            lowerJarDirMergeFileTypeSet.add(jarDirMergeFileType.toLowerCase());
        }
        for (String jarEntryOtherFileType : extensionsManager.getJarEntryOtherFileTypeSet()) {
            String lowerJarEntryOtherFileType = jarEntryOtherFileType.toLowerCase();
            if (JavaCGConstants.FILE_TYPE_CLASS.equals(lowerJarEntryOtherFileType)) {
                continue;
            }
            if (!lowerJarDirMergeFileTypeSet.contains(lowerJarEntryOtherFileType)) {
                logger.error("对jar包中的其他文件解析时需要处理的文件类型 {} 未在合并jar包或目录时需要合并的文件类型配置文件 {} 中指定", jarEntryOtherFileType,
                        JavaCGOtherConfigFileUseSetEnum.OCFUSE_JAR_DIR_MERGE_FILE_TYPE.getFileName());
                return false;
            }
        }
        return true;
    }

    // 打印生成的文件信息
    private void printOutputFileInfo() {
        for (JavaCGOutPutFileTypeEnum javaCGOutPutFileTypeEnum : JavaCGOutPutFileTypeEnum.values()) {
            if (javaCGOutPutFileTypeEnum == JavaCGOutPutFileTypeEnum.OPFTE_ILLEGAL) {
                continue;
            }
            logger.info("写入文件: {}", javaCGOutputInfo.getMainFilePath(javaCGOutPutFileTypeEnum));
        }

        Set<String> otherFileNameSet = javaCGOutputInfo.getOtherFileNameSet();
        if (otherFileNameSet.isEmpty()) {
            logger.info("不写入其他类型的文件");
            return;
        }

        for (String otherFileName : otherFileNameSet) {
            logger.info("写入其他类型的文件 {} {}", otherFileName, javaCGOutputInfo.getOtherFilePath(otherFileName));
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
            logger.error("处理jar包出现异常 {} ", jarFilePath, e);
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

        List<String> springBeanNameList = new ArrayList<>(defineSpringBeanByAnnotationHandler.getSpringBeanNameSet());
        Collections.sort(springBeanNameList);
        for (String springBeanName : springBeanNameList) {
            List<String> springBeanTypeList = defineSpringBeanByAnnotationHandler.getSpringBeanTypeList(springBeanName);
            for (int i = 0; i < springBeanTypeList.size(); i++) {
                JavaCGFileUtil.write2FileWithTab(springBeanWriter, springBeanName, String.valueOf(i), springBeanTypeList.get(i), JavaCGConstants.FILE_KEY_SPRING_BEAN_IN_JAVA);
            }
        }
        // 记录XML中定义的Spring Bean信息
        SpringXmlBeanParserInterface springXmlBeanParser = extensionsManager.getSpringXmlBeanParser();
        if (springXmlBeanParser != null) {
            Map<String, String> springBeanMapInXml = springXmlBeanParser.getBeanMap();
            List<String> springBeanNameInXmlList = new ArrayList<>(springBeanMapInXml.keySet());
            for (String springBeanNameInXml : springBeanNameInXmlList) {
                String springBeanTypeInXml = springBeanMapInXml.get(springBeanNameInXml);
                JavaCGFileUtil.write2FileWithTab(springBeanWriter, springBeanNameInXml, "0", springBeanTypeInXml, JavaCGConstants.FILE_KEY_SPRING_BEAN_IN_XML);
            }
        }
    }

    // 打印重复的类名
    private void printDuplicateClasses() {
        if (duplicateClassNameMap.isEmpty()) {
            logger.info("不存在重复的类名");
            return;
        }

        List<String> duplicateClassNameList = new ArrayList<>(duplicateClassNameMap.keySet());
        Collections.sort(duplicateClassNameList);

        for (String duplicateClassName : duplicateClassNameList) {
            List<String> classFilePathList = duplicateClassNameMap.get(duplicateClassName);
            logger.info("重复的类名 {} 使用的class文件 {}", duplicateClassName, classFilePathList.get(0));
            for (int i = 1; i < classFilePathList.size(); i++) {
                logger.info("重复的类名 {} 跳过的class文件 {}", duplicateClassName, classFilePathList.get(i));
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
        if (codeParser instanceof SpringXmlBeanParserInterface) {
            // 设置对Spring XML中的Bean解析的类
            extensionsManager.setSpringXmlBeanParser((SpringXmlBeanParserInterface) codeParser);
            return;
        }
        extensionsManager.addCodeParser(codeParser);
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
