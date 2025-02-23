package com.adrninistrator.javacg2.entry;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfInfo;
import com.adrninistrator.javacg2.conf.JavaCG2ConfManager;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.dto.classes.ClassExtendsInfo;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg2.dto.jar.OuterInnerJarPath;
import com.adrninistrator.javacg2.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg2.dto.output.JavaCG2OtherRunResult;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.extensions.codeparser.SpringXmlBeanParserInterface;
import com.adrninistrator.javacg2.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg2.handler.ExtendsImplHandler;
import com.adrninistrator.javacg2.handler.MergeJarHandler;
import com.adrninistrator.javacg2.parser.JarEntryHandleParser;
import com.adrninistrator.javacg2.parser.JarEntryPreHandle1Parser;
import com.adrninistrator.javacg2.parser.JarEntryPreHandle2Parser;
import com.adrninistrator.javacg2.spring.DefineSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg2.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2JarUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.adrninistrator.javacg2.writer.WriterSupportSkip;
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
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2021/8/21
 * @description: 入口类
 */
public class JavaCG2Entry {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2Entry.class);

    private final JavaCG2Counter callIdCounter = new JavaCG2Counter(JavaCG2Constants.METHOD_CALL_ID_MIN_BEFORE);
    private final JavaCG2Counter classNumCounter = new JavaCG2Counter();
    private final JavaCG2Counter methodNumCounter = new JavaCG2Counter();
    private final JavaCG2Counter failCounter = new JavaCG2Counter();
    private final JavaCG2Counter fieldRelationshipCounter = new JavaCG2Counter(JavaCG2Constants.RECORD_ID_MIN_BEFORE);

    private final ExtensionsManager extensionsManager = new ExtensionsManager();

    private final AtomicBoolean runFlag = new AtomicBoolean(false);

    private final JavaCG2InputAndOutput javaCG2InputAndOutput = new JavaCG2InputAndOutput();

    private final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    private JarEntryPreHandle1Parser jarEntryPreHandle1Parser;
    private JarEntryPreHandle2Parser jarEntryPreHandle2Parser;
    private JarEntryHandleParser jarEntryHandleParser;

    private ExtendsImplHandler extendsImplHandler;

    /*
        保存需要处理的jar包/目录的路径及对应的序号
        key     jar包/目录路径
        value   jar包/目录对应的序号
     */
    private Map<String, Integer> jarPathNumMap;

    private DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler;

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    public static void main(String[] args) {
        new JavaCG2Entry().run();
    }

    public JavaCG2Entry() {
        javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper(false);
    }

    public JavaCG2Entry(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper) {
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
    }

    public boolean run() {
        if (!runFlag.compareAndSet(false, true)) {
            logger.error("当前类的实例在创建后只能使用一次，假如需要再次执行，请创建新的实例 {}", this.getClass().getSimpleName());
            return false;
        }

        if (javaCG2ConfigureWrapper == null) {
            throw new JavaCG2RuntimeException("配置参数包装对象不允许为null");
        }

        long startTime = System.currentTimeMillis();

        JavaCG2ConfInfo javaCG2ConfInfo = JavaCG2ConfManager.getConfInfo(javaCG2ConfigureWrapper);
        if (javaCG2ConfInfo == null) {
            return false;
        }

        // 处理输出文件路径
        String outputDirPath = handleOutputDir();
        javaCG2ConfInfo.setOutputDirPath(outputDirPath);
        try (JavaCG2ElManager javaCG2ElManager = new JavaCG2ElManager(javaCG2ConfigureWrapper, JavaCG2ElConfigEnum.values(), outputDirPath)) {
            boolean success = parse(javaCG2ElManager, javaCG2ConfInfo);
            logger.info("执行完毕，处理数量，类： {} 方法: {} 方法调用: {} 耗时: {} 秒", classNumCounter.getCount(), methodNumCounter.getCount(), callIdCounter.getCount(),
                    JavaCG2Util.getSpendSeconds(startTime));
            return success;
        }
    }

    private boolean parse(JavaCG2ElManager javaCG2ElManager, JavaCG2ConfInfo javaCG2ConfInfo) {
        JavaCG2Counter jarNumCounter = new JavaCG2Counter(JavaCG2Constants.JAR_NUM_MIN_BEFORE);

        // 处理参数中指定的jar包
        String newJarFilePath = handleJarInConf(jarNumCounter, javaCG2ElManager);

        // 初始化
        if (!init(javaCG2ConfInfo, javaCG2ElManager)) {
            return false;
        }

        JavaCG2OutputInfo javaCG2OutputInfo = javaCG2InputAndOutput.getJavaCG2OutputInfo();
        // 打印生成的文件信息
        printOutputFileInfo(javaCG2OutputInfo);

        try (Writer classAnnotationWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_ANNOTATION));
             Writer javaCG2ConfigWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_JAVACG2_CONFIG));
             Writer classInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_INFO));
             Writer dupClassInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_DUP_CLASS_INFO));
             Writer classReferenceWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_REFERENCE));
             Writer classSignatureGenericsTypeWriter =
                     JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_GENERICS_TYPE));
             Writer classExtImplGenericsTypeWriter =
                     JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_EXT_IMPL_GENERICS_TYPE));
             Writer extendsImplWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_EXTENDS_IMPL));
             Writer fieldAnnotationWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_ANNOTATION));
             Writer fieldInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_INFO));
             Writer fieldGenericsTypeWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_GENERICS_TYPE));
             Writer fieldRelationshipWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_FIELD_RELATIONSHIP));
             Writer getMethodWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_GET_METHOD));
             Writer innerClassWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_INNER_CLASS));
             Writer jarInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_JAR_INFO));
             Writer lambdaMethodInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_LAMBDA_METHOD_INFO));
             Writer methodAnnotationWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ANNOTATION));
             Writer methodArgumentWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ARGUMENT));
             Writer methodArgAnnotationWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ARG_ANNOTATION));
             Writer methodArgGenericsTypeWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ARG_GENERICS_TYPE));
             Writer methodCallInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_INFO));
             Writer methodCallMethodCallReturnWriter =
                     JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_METHOD_CALL_RETURN));
             Writer methodCallStaticFieldWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_STATIC_FIELD));
             Writer methodReturnArgSeqWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_ARG_SEQ));
             Writer methodReturnCallIdWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_CALL_ID));
             Writer methodReturnConstValueWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_CONST_VALUE));
             Writer methodReturnFieldInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_FIELD_INFO));
             Writer methodCallWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL));
             Writer methodInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_INFO));
             Writer dupMethodInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_DUP_METHOD_INFO));
             Writer enumInitArgFieldWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_ENUM_INIT_ARG_FIELD));
             Writer enumInitAssignInfoWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_ENUM_INIT_ASSIGN_INFO));
             Writer methodLineNumberWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_LINE_NUMBER));
             Writer methodReturnGenericsTypeWriter =
                     JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_GENERICS_TYPE));
             Writer methodCatchWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CATCH));
             Writer methodFinallyWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_FINALLY));
             Writer methodThrowWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_THROW));
             Writer setMethodWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_SET_METHOD));
             Writer springBeanWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_BEAN));
             Writer staticFinalFieldMethodCallIdWriter = JavaCG2FileUtil.genBufferedWriter(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_SF_FIELD_METHOD_CALL));
             WriterSupportSkip logMethodSpendTimeWriter = new WriterSupportSkip(javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_LOG_METHOD_SPEND_TIME), false)
        ) {
            jarEntryHandleParser.setClassAnnotationWriter(classAnnotationWriter);
            jarEntryHandleParser.setClassInfoWriter(classInfoWriter);
            jarEntryHandleParser.setDupClassInfoWriter(dupClassInfoWriter);
            jarEntryHandleParser.setClassReferenceWriter(classReferenceWriter);
            jarEntryHandleParser.setClassSignatureGenericsTypeWriter(classSignatureGenericsTypeWriter);
            jarEntryHandleParser.setClassExtImplGenericsTypeWriter(classExtImplGenericsTypeWriter);
            jarEntryHandleParser.setExtendsImplWriter(extendsImplWriter);
            jarEntryHandleParser.setFieldAnnotationWriter(fieldAnnotationWriter);
            jarEntryHandleParser.setFieldInfoWriter(fieldInfoWriter);
            jarEntryHandleParser.setFieldGenericsTypeWriter(fieldGenericsTypeWriter);
            jarEntryHandleParser.setFieldRelationshipWriter(fieldRelationshipWriter);
            jarEntryHandleParser.setGetMethodWriter(getMethodWriter);
            jarEntryHandleParser.setInnerClassWriter(innerClassWriter);
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
            jarEntryHandleParser.setMethodReturnConstValueWriter(methodReturnConstValueWriter);
            jarEntryHandleParser.setMethodReturnFieldInfoWriter(methodReturnFieldInfoWriter);
            jarEntryHandleParser.setMethodCallWriter(methodCallWriter);
            jarEntryHandleParser.setMethodInfoWriter(methodInfoWriter);
            jarEntryHandleParser.setDupMethodInfoWriter(dupMethodInfoWriter);
            jarEntryHandleParser.setEnumInitArgFieldWriter(enumInitArgFieldWriter);
            jarEntryHandleParser.setEnumInitAssignInfoWriter(enumInitAssignInfoWriter);
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

            // 记录处理的jar包信息
            Map<Integer, String> jarNumPathMap = new HashMap<>(jarPathNumMap.size());
            for (Map.Entry<String, Integer> entry : jarPathNumMap.entrySet()) {
                jarNumPathMap.put(entry.getValue(), entry.getKey());
            }
            for (int i = JavaCG2Constants.JAR_NUM_MIN; i <= jarNumPathMap.size(); i++) {
                String jarDirPath = jarNumPathMap.get(i);
                String jarDirType;
                OuterInnerJarPath outerInnerJarPath = JavaCG2JarUtil.parseOuterInnerJarPath(jarDirPath);
                String outerJarDirPath;
                String innerJarPath = outerInnerJarPath.getInnerJarPath();
                if (innerJarPath != null) {
                    outerJarDirPath = outerInnerJarPath.getOuterJarPath();
                    jarDirType = JavaCG2Constants.FILE_KEY_JAR_IN_JAR;
                } else {
                    outerJarDirPath = jarDirPath;
                    boolean isJar = new File(jarDirPath).isFile();
                    jarDirType = isJar ? JavaCG2Constants.FILE_KEY_JAR : JavaCG2Constants.FILE_KEY_DIR;
                }
                JavaCG2FileUtil.write2FileWithTab(jarInfoWriter, jarDirType, String.valueOf(i), outerJarDirPath, innerJarPath);
            }

            JavaCG2FileUtil.write2FileWithTab(jarInfoWriter, JavaCG2Constants.FILE_KEY_RESULT_DIR_INFO_PREFIX, String.valueOf(jarNumPathMap.size() + 1),
                    javaCG2OutputInfo.getOutputDirPath(), "");

            // 记录java-callgraph2组件使用的配置参数
            recordJavaCG2Config(javaCG2ConfigWriter);
            return true;
        } catch (Exception e) {
            logger.error("出现异常 ", e);
            return false;
        } finally {
            // 关闭扩展类管理类
            extensionsManager.close();
        }
    }

    // 处理输出文件路径
    private String handleOutputDir() {
        MergeJarHandler mergeJarHandler = new MergeJarHandler(javaCG2ConfigureWrapper, null, null, null, true);
        File newJarFile = mergeJarHandler.mergeJar();
        if (newJarFile == null) {
            throw new JavaCG2RuntimeException("获取处理需要解析的jar文件/目录的输出文件路径失败");
        }

        String newJarFilePath = JavaCG2FileUtil.getCanonicalPath(newJarFile);
        String outputRootPath = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH);
        String outputDirPath;
        if (StringUtils.isBlank(outputRootPath)) {
            // 配置参数中未指定生成文件的根目录，生成在jar包所在目录
            outputDirPath = newJarFilePath + JavaCG2Constants.DIR_TAIL_OUTPUT + File.separator;
        } else {
            // 配置参数中有指定生成文件的根目录，生成在指定目录
            outputDirPath = JavaCG2Util.addSeparator4FilePath(outputRootPath) + newJarFile.getName() + JavaCG2Constants.DIR_TAIL_OUTPUT + File.separator;
        }
        logger.info("当前输出的根目录: {}", outputDirPath);
        if (!JavaCG2FileUtil.isDirectoryExists(outputDirPath, true)) {
            throw new JavaCG2RuntimeException("创建输出目录失败");
        }
        return outputDirPath;
    }

    // 处理配置参数中指定的jar包
    private String handleJarInConf(JavaCG2Counter jarNumCounter, JavaCG2ElManager javaCG2ElManager) {
        jarPathNumMap = new HashMap<>();

        // 对指定的jar包进行处理
        MergeJarHandler mergeJarHandler = new MergeJarHandler(javaCG2ConfigureWrapper, jarPathNumMap, jarNumCounter, javaCG2ElManager, false);
        File newJarFile = mergeJarHandler.mergeJar();
        if (newJarFile == null) {
            throw new JavaCG2RuntimeException("处理需要解析的jar文件/目录失败");
        }

        String newJarFilePath = JavaCG2FileUtil.getCanonicalPath(newJarFile);
        logger.info("实际处理的jar文件: {}", newJarFilePath);
        return newJarFilePath;
    }

    // 初始化
    private boolean init(JavaCG2ConfInfo javaCG2ConfInfo, JavaCG2ElManager javaCG2ElManager) {
        // 检查方法调用枚举类型是否重复定义
        JavaCG2CallTypeEnum.checkRepeat();

        String outputFileExt = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT);
        // 处理结果信息相关
        JavaCG2OutputInfo javaCG2OutputInfo = new JavaCG2OutputInfo(javaCG2ConfInfo.getOutputDirPath(), outputFileExt);

        JavaCG2OtherRunResult javaCG2OtherRunResult = new JavaCG2OtherRunResult();
        javaCG2InputAndOutput.setJavaCG2ConfInfo(javaCG2ConfInfo);
        javaCG2InputAndOutput.setJavaCG2ConfigureWrapper(javaCG2ConfigureWrapper);
        javaCG2InputAndOutput.setJavaCG2ElManager(javaCG2ElManager);
        javaCG2InputAndOutput.setJavaCG2OutputInfo(javaCG2OutputInfo);
        javaCG2InputAndOutput.setJavaCG2OtherRunResult(javaCG2OtherRunResult);

        // 扩展类管理类初始化
        extensionsManager.setJavaCG2InputAndOutput(javaCG2InputAndOutput);
        if (!extensionsManager.init()) {
            return false;
        }

        // 第一次预处理相关
        // Runnable实现类Map
        Map<String, Boolean> runnableImplClassMap = new HashMap<>(JavaCG2Constants.SIZE_100);
        // Callable实现类Map
        Map<String, Boolean> callableImplClassMap = new HashMap<>(JavaCG2Constants.SIZE_100);
        // TransactionCallback实现类Map
        Map<String, Boolean> transactionCallbackImplClassMap = new HashMap<>(JavaCG2Constants.SIZE_10);
        // TransactionCallbackWithoutResult子类Map
        Map<String, Boolean> transactionCallbackWithoutResultChildClassMap = new HashMap<>(JavaCG2Constants.SIZE_10);
        // Thread子类Map
        Map<String, Boolean> threadChildClassMap = new HashMap<>(JavaCG2Constants.SIZE_100);
        // 类名及对应的父类（父类非Object时记录）
        Map<String, String> classAndSuperMap = new HashMap<>(JavaCG2Constants.SIZE_100);
        // 涉及继承的接口名Set
        Set<String> interfaceExtendsSet = new HashSet<>(JavaCG2Constants.SIZE_100);
        // 所有在jar包中出现的类名Set
        Set<String> allClassNameSet = new HashSet<>(JavaCG2Constants.SIZE_1000);
  
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
        Map<String, List<String>> classImplementsInfoMap = new HashMap<>(JavaCG2Constants.SIZE_200);

        /*
            涉及继承（父类非Object）、实现的类中，可能涉及继承（父类与子类）、实现（实现类）的方法
            key     类名
            value   相关方法
                key     方法名称、参数、返回值
                value   方法access_flags
         */
        Map<String, Map<MethodArgReturnTypes, Integer>> classExtendsImplMethodWithArgTypesMap = new HashMap<>(JavaCG2Constants.SIZE_100);

        /*
            接口中的方法信息
            key     接口名
            value   接口中的方法信息
        */
        Map<String, Map<MethodArgReturnTypes, Integer>> interfaceMethodWithArgTypesMap = new HashMap<>(JavaCG2Constants.SIZE_200);

        if (Boolean.TRUE.equals(javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE))) {
            defineSpringBeanByAnnotationHandler = new DefineSpringBeanByAnnotationHandler(javaCG2InputAndOutput, failCounter);
        }
        boolean onlyOneJar = jarPathNumMap.size() == 1;
        jarEntryPreHandle1Parser = new JarEntryPreHandle1Parser(javaCG2InputAndOutput, onlyOneJar, defineSpringBeanByAnnotationHandler, extensionsManager);
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
        Map<String, ClassExtendsInfo> classExtendsInfoMap = new HashMap<>(JavaCG2Constants.SIZE_100);

        /*
            父类对应的子类信息
            key     父类类名
            value   子类类名列表
         */
        Map<String, List<String>> childrenClassMap = new HashMap<>(JavaCG2Constants.SIZE_100);

        /*
            接口涉及继承的信息
            key     类名
            value   接口继承的信息
        */
        Map<String, List<String>> interfaceExtendsInfoMap = new HashMap<>(JavaCG2Constants.SIZE_100);

        /*
            父接口对应的子接口信息
            key     父接口类名
            value   子接口类名列表
         */
        Map<String, List<String>> childrenInterfaceMap = new HashMap<>(JavaCG2Constants.SIZE_100);

        if (javaCG2ConfInfo.isParseMethodCallTypeValue()) {
            useSpringBeanByAnnotationHandler = new UseSpringBeanByAnnotationHandler(
                    classExtendsInfoMap,
                    classImplementsInfoMap,
                    interfaceExtendsInfoMap,
                    defineSpringBeanByAnnotationHandler,
                    extensionsManager.getSpringXmlBeanParser());
        }
        jarEntryPreHandle2Parser = new JarEntryPreHandle2Parser(javaCG2InputAndOutput, onlyOneJar, useSpringBeanByAnnotationHandler);
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
        jarEntryHandleParser = new JarEntryHandleParser(javaCG2InputAndOutput, onlyOneJar);
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
        extendsImplHandler = new ExtendsImplHandler(javaCG2InputAndOutput);
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

    // 打印生成的文件信息
    private void printOutputFileInfo(JavaCG2OutputInfo javaCG2OutputInfo) {
        for (JavaCG2OutPutFileTypeEnum javaCG2OutPutFileTypeEnum : JavaCG2OutPutFileTypeEnum.values()) {
            if (javaCG2OutPutFileTypeEnum == JavaCG2OutPutFileTypeEnum.OPFTE_ILLEGAL) {
                continue;
            }
            logger.info("写入文件: {}", javaCG2OutputInfo.getMainFilePath(javaCG2OutPutFileTypeEnum));
        }

        Set<String> otherFileNameSet = javaCG2OutputInfo.getOtherFileNameSet();
        if (otherFileNameSet.isEmpty()) {
            logger.info("不写入其他类型的文件");
            return;
        }

        for (String otherFileName : otherFileNameSet) {
            logger.info("写入其他类型的文件 {} {}", otherFileName, javaCG2OutputInfo.getOtherFilePath(otherFileName));
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

            // 处理继承及实现相关的方法
            extendsImplHandler.setMethodCallWriter(methodCallWriter);
            extendsImplHandler.handle();

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
                JavaCG2FileUtil.write2FileWithTab(springBeanWriter, springBeanName, String.valueOf(i), springBeanTypeList.get(i), JavaCG2Constants.FILE_KEY_SPRING_BEAN_IN_JAVA);
            }
        }
        // 记录XML中定义的Spring Bean信息
        SpringXmlBeanParserInterface springXmlBeanParser = extensionsManager.getSpringXmlBeanParser();
        if (springXmlBeanParser != null) {
            Map<String, String> springBeanMapInXml = springXmlBeanParser.getBeanMap();
            List<String> springBeanNameInXmlList = new ArrayList<>(springBeanMapInXml.keySet());
            for (String springBeanNameInXml : springBeanNameInXmlList) {
                String springBeanTypeInXml = springBeanMapInXml.get(springBeanNameInXml);
                JavaCG2FileUtil.write2FileWithTab(springBeanWriter,
                        springBeanNameInXml,
                        "0",
                        springBeanTypeInXml,
                        JavaCG2Constants.FILE_KEY_SPRING_BEAN_IN_XML);
            }
        }
    }

    // 记录java-callgraph2组件使用的配置参数
    private void recordJavaCG2Config(Writer javaCG2ConfigWriter) throws IOException {
        for (JavaCG2ConfigKeyEnum javaCG2ConfigKeyEnum : JavaCG2ConfigKeyEnum.values()) {
            Object value = javaCG2ConfigureWrapper.getMainConfig(javaCG2ConfigKeyEnum, false);
            String printValue = JavaCG2Util.getObjectPrintValue(value);
            JavaCG2FileUtil.write2FileWithTab(javaCG2ConfigWriter, javaCG2ConfigKeyEnum.getFileName(), javaCG2ConfigKeyEnum.getKey(), printValue,
                    JavaCG2Constants.CONFIG_PROPERTIES);
        }

        JavaCG2OtherConfigFileUseListEnum[] javaCG2OtherConfigFileUseListEnums = JavaCG2OtherConfigFileUseListEnum.values();
        for (JavaCG2OtherConfigFileUseListEnum currentConfig : javaCG2OtherConfigFileUseListEnums) {
            List<String> configValueList;
            if (JavaCG2OtherConfigFileUseListEnum.OCFULE_CODE_PARSER_ONLY_4SHOW == currentConfig) {
                configValueList = extensionsManager.getAllCodeParserNameList();
            } else {
                configValueList = javaCG2ConfigureWrapper.getOtherConfigList(currentConfig, false);
            }
            int seq = 0;
            for (String configValue : configValueList) {
                JavaCG2FileUtil.write2FileWithTab(javaCG2ConfigWriter, currentConfig.getKey(), String.valueOf(seq), configValue, JavaCG2Constants.CONFIG_LIST);
                seq++;
            }
        }

        JavaCG2OtherConfigFileUseSetEnum[] javaCG2OtherConfigFileUseSetEnums = JavaCG2OtherConfigFileUseSetEnum.values();
        for (JavaCG2OtherConfigFileUseSetEnum currentConfig : javaCG2OtherConfigFileUseSetEnums) {
            Set<String> configSet = javaCG2ConfigureWrapper.getOtherConfigSet(currentConfig, false);
            List<String> configValueList = new ArrayList<>(configSet);
            // 排序后打印
            Collections.sort(configValueList);
            int seq = 0;
            for (String configValue : configValueList) {
                JavaCG2FileUtil.write2FileWithTab(javaCG2ConfigWriter, currentConfig.getKey(), String.valueOf(seq), configValue, JavaCG2Constants.CONFIG_SET);
                seq++;
            }
        }
    }

    // 获取扩展类管理类
    public ExtensionsManager getExtensionsManager() {
        return extensionsManager;
    }

    // 获取java-callgraph2的输入与输出数据
    public JavaCG2InputAndOutput getJavaCG2InputAndOutput() {
        return javaCG2InputAndOutput;
    }
}
