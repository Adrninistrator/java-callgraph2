package com.adrninistrator.javacg2.parser;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfInfo;
import com.adrninistrator.javacg2.dto.classes.InnerClassInfo;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg2.dto.type.JavaCG2GenericsType;
import com.adrninistrator.javacg2.extensions.annotationattributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg2.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg2.handler.ClassHandler;
import com.adrninistrator.javacg2.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2GenericsTypeUtil;
import com.adrninistrator.javacg2.util.JavaCG2JarUtil;
import com.adrninistrator.javacg2.writer.WriterSupportSkip;
import copy.javassist.bytecode.BadBytecode;
import copy.javassist.bytecode.SignatureAttribute;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Signature;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，正式处理
 */
public class JarEntryHandleParser extends AbstractJarEntryParser {

    private static final Logger logger = LoggerFactory.getLogger(JarEntryHandleParser.class);

    /*
        记录已处理过的类名及次数
        key 类名
        value   处理次数
     */
    private final Map<String, JavaCG2Counter> handledClassNameTimesMap = new HashMap<>();

    /*
        已记录的包名
            key 包名
            value   包含包名的jar序号
     */
    private final Map<String, Set<Integer>> recordedPackageNameMap = new HashMap<>();

    private final boolean parseJarCompatibilityMode;
    private final boolean parseOnlyClassMode;

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;

    private Writer classAnnotationWriter;
    private Writer classExtImplGenericsTypeWriter;
    private Writer classInfoWriter;
    private Writer classReferenceWriter;
    private Writer dupClassReferenceWriter;
    private Writer classSignatureGenericsTypeWriter;
    private Writer dupClassInfoWriter;
    private Writer dupMethodInfoWriter;
    private Writer dupFieldInfoWriter;
    private Writer enumInitArgFieldWriter;
    private Writer enumInitAssignInfoWriter;
    private Writer extendsImplWriter;
    private Writer fieldAnnotationWriter;
    private Writer fieldGenericsTypeWriter;
    private Writer fieldInfoWriter;
    private Writer fieldRelationshipWriter;
    private Writer fieldUsageOtherWriter;
    private Writer getMethodWriter;
    private Writer innerClassWriter;
    private Writer lambdaMethodInfoWriter;
    private Writer methodAnnotationWriter;
    private Writer methodArgAnnotationWriter;
    private Writer methodArgGenericsTypeWriter;
    private Writer methodArgumentWriter;
    private Writer methodCallInfoWriter;
    private Writer methodCallMethodCallReturnWriter;
    private Writer methodCallStaticFieldWriter;
    private Writer methodCallNonStaticFieldWriter;
    private Writer methodCallStaticFieldMCRWriter;
    private Writer methodCallWriter;
    private Writer methodCallRawCalleeWriter;
    private Writer methodCatchWriter;
    private Writer methodFinallyWriter;
    private Writer methodInfoWriter;
    private Writer methodLineNumberWriter;
    private Writer methodReturnArgSeqWriter;
    private Writer methodReturnCallIdWriter;
    private Writer methodReturnConstValueWriter;
    private Writer methodReturnFieldInfoWriter;
    private Writer methodReturnGenericsTypeWriter;
    private Writer methodThrowWriter;
    private Writer packageInfoWriter;
    private Writer setMethodWriter;
    private Writer staticFinalFieldMethodCallIdWriter;

    private WriterSupportSkip logMethodSpendTimeWriter;

    // 扩展类管理类
    private ExtensionsManager extensionsManager;

    private JavaCG2Counter callIdCounter;
    private JavaCG2Counter classNumCounter;
    private JavaCG2Counter methodNumCounter;
    private JavaCG2Counter failCounter;
    private JavaCG2Counter fieldRelationshipCounter;

    private ClassAndJarNum classAndJarNum;

    private AnnotationAttributesFormatterInterface annotationAttributesFormatter;

    public JarEntryHandleParser(JavaCG2InputAndOutput javaCG2InputAndOutput, boolean onlyOneJar) {
        super(javaCG2InputAndOutput, onlyOneJar);
        JavaCG2ConfInfo javaCG2ConfInfo = javaCG2InputAndOutput.getJavaCG2ConfInfo();
        parseJarCompatibilityMode = javaCG2ConfInfo.isParseJarCompatibilityMode();
        parseOnlyClassMode = javaCG2ConfInfo.isParseOnlyClassMode();
    }

    @Override
    protected boolean handleEntry(ZipInputStream zipInputStream, String jarEntryPath) throws IOException {
        if (!JavaCG2FileUtil.checkClassFile(jarEntryPath)) {
            // 非class文件则跳过
            return true;
        }

        JavaClass javaClass = new ClassParser(zipInputStream, jarEntryPath).parse();
//        if (!"xxx".equals(javaClass.getClassName())) {
//            return true;
//        }

        // 处理jar包中的class文件
        return handleClassEntry(javaClass, jarEntryPath);
    }

    @Override
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryPath) throws IOException {
        if (JavaCG2Constants.MODULE_INFO_CLASS.equals(jarEntryPath) || jarEntryPath.endsWith(JavaCG2Constants.MODULE_INFO_CLASS_SUFFIX)) {
            // 跳过 module-info.class
            return true;
        }

        // 处理Java类
        String className = javaClass.getClassName();
        if (javaCG2ElManager.checkIgnoreParseClass(className)) {
            logger.debug("跳过解析类 {}", className);
            return true;
        }

        JavaCG2Counter classHandledTimes = handledClassNameTimesMap.computeIfAbsent(className, k -> new JavaCG2Counter());
        // 判断是否为重复的类
        boolean duplicateClass = false;
        if (classHandledTimes.getCount() > 0) {
            logger.debug("处理重复同名Class: {}", className);
            duplicateClass = true;
        }
        classHandledTimes.addAndGet();

        logger.debug("处理Class: {}", className);

        int classJarNum;
        if (onlyOneJar) {
            classJarNum = JavaCG2Constants.JAR_NUM_MIN;
        } else {
            classJarNum = JavaCG2JarUtil.getJarNumFromDirName(lastFirstDirName);
        }

        // 记录包名
        recordPackageName(javaClass.getPackageName());

        ClassHandler classHandler = genClassHandler(javaClass, jarEntryPath, classJarNum);
        classNumCounter.addAndGet();
        // 开始处理当前类
        // 处理内部类信息
        handleInnerClass(javaClass);

        // 处理特殊模式
        if (handleSpecialMode(javaClass, duplicateClass, classJarNum, jarEntryPath, classHandler)) {
            return true;
        }

        int failCountBefore = failCounter.getCount();
        boolean success;
        if (duplicateClass) {
            success = true;
            // 处理重复类
            classHandler.handleDuplicateClass();
        } else {
            // 处理非重复的类
            success = classHandler.handleClass();
        }
        if (failCounter.getCount() > failCountBefore) {
            // 将处理失败的类保存到目录中
            saveHandleFailClass(javaClass);
        }

        // 记录类的信息
        recordClassInfo(javaClass, duplicateClass, className, classJarNum, jarEntryPath);

        if (duplicateClass) {
            // 重复类不执行后续处理
            return true;
        }

        // 记录继承及实现相关信息
        recordExtendsAndImplInfo(javaClass, className);

        // 处理类的签名
        handleClassSignature(javaClass, className);
        return success;
    }

    // 记录包名
    private void recordPackageName(String packageName) throws IOException {
        // 执行记录包名
        if (!doRecordPackageName(packageName)) {
            return;
        }
        // 对每个层级的包名进行记录
        int lastIndex = 0;
        while (true) {
            int index = packageName.indexOf(JavaCG2Constants.FLAG_DOT, lastIndex);
            if (index == -1) {
                break;
            }
            String upperPackageName = packageName.substring(0, index);
            lastIndex = index + 1;
            // 记录上层包名
            doRecordPackageName(upperPackageName);
        }
    }

    // 执行记录包名
    private boolean doRecordPackageName(String packageName) throws IOException {
        Set<Integer> containsPackageJarNumSet = recordedPackageNameMap.computeIfAbsent(packageName, k -> new HashSet<>());
        if (containsPackageJarNumSet.add(lastJarNum)) {
            // 未记录时记录包名
            int packageLevel = StringUtils.countMatches(packageName, JavaCG2Constants.FLAG_DOT) + 1;
            JavaCG2FileUtil.write2FileWithTab(packageInfoWriter, packageName, String.valueOf(packageLevel), String.valueOf(lastJarNum), lastJarFileName);
            return true;
        }
        return false;
    }

    // 处理特殊模式
    private boolean handleSpecialMode(JavaClass javaClass, boolean duplicateClass, int classJarNum, String jarEntryPath, ClassHandler classHandler) throws IOException {
        String className = javaClass.getClassName();
        if (parseOnlyClassMode) {
            // 记录类的信息
            recordClassInfo(javaClass, duplicateClass, className, classJarNum, jarEntryPath);
            // 记录类引用的类
            classHandler.recordReferencedClass(duplicateClass);
            if (!duplicateClass) {
                // 非重复类时，记录类上的注解信息
                recordClassAnnotation(javaClass);
            }
            return true;
        }

        if (parseJarCompatibilityMode) {
            // 使用Jar兼容性检查模式
            // 记录类的信息
            recordClassInfo(javaClass, duplicateClass, className, classJarNum, jarEntryPath);

            // 使用Jar兼容性检查模式，仅解析基础信息时，处理重复类中的方法
            classHandler.parseCompatibilityMode(duplicateClass);

            if (!duplicateClass) {
                // 非重复类时，记录继承及实现相关信息
                recordExtendsAndImplInfo(javaClass, className);

                // 记录类上的注解信息
                recordClassAnnotation(javaClass);
            }
            return true;
        }
        return false;
    }

    // 记录类上的注解信息
    private void recordClassAnnotation(JavaClass javaClass) {
        JavaCG2AnnotationUtil.writeAnnotationInfo(classAnnotationWriter, javaClass.getAnnotationEntries(), annotationAttributesFormatter, javaClass.getClassName());
    }

    private ClassHandler genClassHandler(JavaClass javaClass, String jarEntryPath, int classJarNum) {
        ClassHandler classHandler = new ClassHandler(javaClass, jarEntryPath, javaCG2InputAndOutput, classJarNum);
        classHandler.setUseSpringBeanByAnnotationHandler(useSpringBeanByAnnotationHandler);
        classHandler.setRunnableImplClassMap(runnableImplClassMap);
        classHandler.setCallableImplClassMap(callableImplClassMap);
        classHandler.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        classHandler.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        classHandler.setThreadChildClassMap(threadChildClassMap);
        classHandler.setCallIdCounter(callIdCounter);
        classHandler.setClassAnnotationWriter(classAnnotationWriter);
        classHandler.setClassReferenceWriter(classReferenceWriter);
        classHandler.setDupClassReferenceWriter(dupClassReferenceWriter);
        classHandler.setDupMethodInfoWriter(dupMethodInfoWriter);
        classHandler.setDupFieldInfoWriter(dupFieldInfoWriter);
        classHandler.setEnumInitArgFieldWriter(enumInitArgFieldWriter);
        classHandler.setEnumInitAssignInfoWriter(enumInitAssignInfoWriter);
        classHandler.setFieldAnnotationWriter(fieldAnnotationWriter);
        classHandler.setFieldGenericsTypeWriter(fieldGenericsTypeWriter);
        classHandler.setFieldInfoWriter(fieldInfoWriter);
        classHandler.setFieldRelationshipWriter(fieldRelationshipWriter);
        classHandler.setFieldUsageOtherWriter(fieldUsageOtherWriter);
        classHandler.setGetMethodWriter(getMethodWriter);
        classHandler.setLambdaMethodInfoWriter(lambdaMethodInfoWriter);
        classHandler.setLogMethodSpendTimeWriter(logMethodSpendTimeWriter);
        classHandler.setMethodAnnotationWriter(methodAnnotationWriter);
        classHandler.setMethodArgAnnotationWriter(methodArgAnnotationWriter);
        classHandler.setMethodArgGenericsTypeWriter(methodArgGenericsTypeWriter);
        classHandler.setMethodArgumentWriter(methodArgumentWriter);
        classHandler.setMethodCallInfoWriter(methodCallInfoWriter);
        classHandler.setMethodCallMethodCallReturnWriter(methodCallMethodCallReturnWriter);
        classHandler.setMethodCallStaticFieldWriter(methodCallStaticFieldWriter);
        classHandler.setMethodCallNonStaticFieldWriter(methodCallNonStaticFieldWriter);
        classHandler.setMethodCallStaticFieldMCRWriter(methodCallStaticFieldMCRWriter);
        classHandler.setMethodCallWriter(methodCallWriter);
        classHandler.setMethodCallRawCalleeWriter(methodCallRawCalleeWriter);
        classHandler.setMethodCatchWriter(methodCatchWriter);
        classHandler.setMethodFinallyWriter(methodFinallyWriter);
        classHandler.setMethodInfoWriter(methodInfoWriter);
        classHandler.setMethodLineNumberWriter(methodLineNumberWriter);
        classHandler.setMethodReturnArgSeqWriter(methodReturnArgSeqWriter);
        classHandler.setMethodReturnCallIdWriter(methodReturnCallIdWriter);
        classHandler.setMethodReturnConstValueWriter(methodReturnConstValueWriter);
        classHandler.setMethodReturnFieldInfoWriter(methodReturnFieldInfoWriter);
        classHandler.setMethodReturnGenericsTypeWriter(methodReturnGenericsTypeWriter);
        classHandler.setMethodThrowWriter(methodThrowWriter);
        classHandler.setSetMethodWriter(setMethodWriter);
        classHandler.setStaticFinalFieldMethodCallIdWriter(staticFinalFieldMethodCallIdWriter);
        classHandler.recordExtensionsManager(extensionsManager);
        classHandler.setMethodNumCounter(methodNumCounter);
        classHandler.setFailCounter(failCounter);
        classHandler.setFieldRelationshipCounter(fieldRelationshipCounter);
        classHandler.setClassAndJarNum(classAndJarNum);
        return classHandler;
    }

    // 记录类的信息
    private void recordClassInfo(JavaClass javaClass, boolean duplicateClass, String className, int classJarNum, String jarEntryPath) throws IOException {
        String classMd5 = DigestUtils.md5Hex(javaClass.getBytes());
        Writer usedClassInfoWriter = duplicateClass ? dupClassInfoWriter : classInfoWriter;
        JavaCG2FileUtil.write2FileWithTab(usedClassInfoWriter, className, String.valueOf(javaClass.getAccessFlags()), classMd5, String.valueOf(classJarNum), jarEntryPath);
    }

    // 将处理失败的类保存到目录中
    private void saveHandleFailClass(JavaClass javaClass) {
        String saveClassFilePath = javaCG2InputAndOutput.getJavaCG2ConfInfo().getOutputDirPath() + JavaCG2DirEnum.IDE_FAIL_CLASSES + File.separator +
                javaClass.getClassName() + JavaCG2Constants.EXT_CLASS;
        File saveClassFile = new File(saveClassFilePath);
        logger.info("将处理失败的class文件保存到文件 {}", saveClassFile.getAbsolutePath());
        try {
            // 以下文件有创建文件所在目录
            javaClass.dump(saveClassFile);
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 记录继承及实现相关信息
    private void recordExtendsAndImplInfo(JavaClass javaClass, String className) throws IOException {
        String superClassName = javaClass.getSuperclassName();
        String accessFlagsStr = String.valueOf(javaClass.getAccessFlags());
        if (!JavaCG2ClassMethodUtil.isObjectClass(superClassName)) {
            // 仅处理父类非Object类的情况
            JavaCG2FileUtil.write2FileWithTab(extendsImplWriter,
                    className,
                    accessFlagsStr,
                    JavaCG2Constants.FILE_KEY_EXTENDS,
                    superClassName);
        }

        for (String interfaceName : javaClass.getInterfaceNames()) {
            // 接口不会是Object类，不需要判断
            JavaCG2FileUtil.write2FileWithTab(extendsImplWriter,
                    className,
                    accessFlagsStr,
                    JavaCG2Constants.FILE_KEY_IMPLEMENTS,
                    interfaceName);
        }
    }

    // 处理类的签名
    private void handleClassSignature(JavaClass javaClass, String className) throws IOException {
        if (javaClass.isAnnotation()) {
            // 若当前类为注解则不处理
            return;
        }

        Signature signature = JavaCG2ByteCodeUtil.getSignatureOfClass(javaClass);
        if (signature == null) {
            return;
        }

        try {
            SignatureAttribute.ClassSignature signatureAttribute = SignatureAttribute.toClassSignature(signature.getSignature());

            // 类涉及继承与实现的签名信息Map
            SignatureAttribute.TypeParameter[] params = signatureAttribute.getParameters();
            if (ArrayUtils.isNotEmpty(params)) {
                // 处理类签名中的泛型类型
                int seq = -1;
                for (SignatureAttribute.TypeParameter param : params) {
                    seq++;
                    String typeVariablesName = param.getName();
                    SignatureAttribute.ObjectType paramClassBound = param.getClassBound();
                    if (paramClassBound != null) {
                        // 泛型类型继承了特定类的处理
                        String extendsClassName = paramClassBound.jvmTypeName();
                        JavaCG2FileUtil.write2FileWithTab(classSignatureGenericsTypeWriter, className, String.valueOf(seq), typeVariablesName, extendsClassName);
                        continue;
                    }
                    SignatureAttribute.ObjectType[] paramInterfaceBounds = param.getInterfaceBound();
                    if (ArrayUtils.isNotEmpty(paramInterfaceBounds)) {
                        // 泛型类型继承了特定接口的处理
                        String extendsClassName = paramInterfaceBounds[0].jvmTypeName();
                        JavaCG2FileUtil.write2FileWithTab(classSignatureGenericsTypeWriter, className, String.valueOf(seq), typeVariablesName, extendsClassName);
                    }
                }
            }

            // 处理父类相关的签名
            SignatureAttribute.ClassType superClassType = signatureAttribute.getSuperClass();
            if (superClassType != null) {
                String superClassName = JavaCG2GenericsTypeUtil.getClassName(superClassType);
                // 记录类继承中的泛型信息
                recordExtImplGenericsInfo(JavaCG2Constants.FILE_KEY_EXTENDS, className, superClassName, superClassType);
            }

            // 处理接口相关的签名
            SignatureAttribute.ClassType[] interfaceClassTypes = signatureAttribute.getInterfaces();
            if (interfaceClassTypes != null) {
                for (SignatureAttribute.ClassType interfaceClassType : interfaceClassTypes) {
                    String interfaceClassName = JavaCG2GenericsTypeUtil.getClassName(interfaceClassType);
                    // 记录类实现中的泛型信息
                    recordExtImplGenericsInfo(JavaCG2Constants.FILE_KEY_IMPLEMENTS, className, interfaceClassName, interfaceClassType);
                }
            }
        } catch (BadBytecode e) {
            logger.error("处理类的签名出现异常 {} ", className, e);
        }
    }

    // 处理内部类信息
    private void handleInnerClass(JavaClass javaClass) throws IOException {
        // 获取类中的内部类信息
        List<InnerClassInfo> innerClassInfoList = JavaCG2ByteCodeUtil.getInnerClassInfo(javaClass);
        for (InnerClassInfo innerClassInfo : innerClassInfoList) {
            JavaCG2FileUtil.write2FileWithTab(innerClassWriter, innerClassInfo.getInnerClassName(), innerClassInfo.getOuterClassName(),
                    JavaCG2YesNoEnum.parseStrValue(innerClassInfo.isAnonymousClass()));
        }
    }

    /**
     * 记录类继承或实现中的泛型信息
     *
     * @param extendsOrImplStr
     * @param className
     * @param superOrInterfaceName
     * @param classType
     */
    private void recordExtImplGenericsInfo(String extendsOrImplStr, String className, String superOrInterfaceName, SignatureAttribute.ClassType classType) throws IOException {
        SignatureAttribute.TypeArgument[] arguments = classType.getTypeArguments();
        if (arguments == null) {
            return;
        }

        int seq = 0;
        for (SignatureAttribute.TypeArgument typeArgument : arguments) {
            SignatureAttribute.ObjectType objectType = typeArgument.getType();

            List<JavaCG2GenericsType> classGenericsTypeList = new ArrayList<>();
            // 解析类签名中的泛型类型
            JavaCG2GenericsTypeUtil.parseTypeDefineGenericsType(objectType, true, classGenericsTypeList);

            String seqStr = String.valueOf(seq);
            for (int genericsSeq = 0; genericsSeq < classGenericsTypeList.size(); genericsSeq++) {
                JavaCG2GenericsType javaCG2GenericsType = classGenericsTypeList.get(genericsSeq);
                // 记录当前类/接口继承或实现时与父类或接口相同的泛型信息
                JavaCG2FileUtil.write2FileWithTab(classExtImplGenericsTypeWriter,
                        className,
                        extendsOrImplStr,
                        seqStr,
                        superOrInterfaceName,
                        JavaCG2GenericsTypeUtil.genGenericsTypeStr4ClassExtImpl(genericsSeq, javaCG2GenericsType));
            }
            seq++;
        }
    }

    public void recordExtensionsManager(ExtensionsManager extensionsManager) {
        this.extensionsManager = extensionsManager;
        annotationAttributesFormatter = extensionsManager.getAnnotationAttributesFormatter();
    }

    //
    public void setUseSpringBeanByAnnotationHandler(UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler) {
        this.useSpringBeanByAnnotationHandler = useSpringBeanByAnnotationHandler;
    }

    public void setRunnableImplClassMap(Map<String, Boolean> runnableImplClassMap) {
        this.runnableImplClassMap = runnableImplClassMap;
    }

    public void setCallableImplClassMap(Map<String, Boolean> callableImplClassMap) {
        this.callableImplClassMap = callableImplClassMap;
    }

    public void setTransactionCallbackImplClassMap(Map<String, Boolean> transactionCallbackImplClassMap) {
        this.transactionCallbackImplClassMap = transactionCallbackImplClassMap;
    }

    public void setTransactionCallbackWithoutResultChildClassMap(Map<String, Boolean> transactionCallbackWithoutResultChildClassMap) {
        this.transactionCallbackWithoutResultChildClassMap = transactionCallbackWithoutResultChildClassMap;
    }

    public void setThreadChildClassMap(Map<String, Boolean> threadChildClassMap) {
        this.threadChildClassMap = threadChildClassMap;
    }

    public void setClassAnnotationWriter(Writer classAnnotationWriter) {
        this.classAnnotationWriter = classAnnotationWriter;
    }

    public void setClassExtImplGenericsTypeWriter(Writer classExtImplGenericsTypeWriter) {
        this.classExtImplGenericsTypeWriter = classExtImplGenericsTypeWriter;
    }

    public void setClassInfoWriter(Writer classInfoWriter) {
        this.classInfoWriter = classInfoWriter;
    }

    public void setClassReferenceWriter(Writer classReferenceWriter) {
        this.classReferenceWriter = classReferenceWriter;
    }

    public void setDupClassReferenceWriter(Writer dupClassReferenceWriter) {
        this.dupClassReferenceWriter = dupClassReferenceWriter;
    }

    public void setClassSignatureGenericsTypeWriter(Writer classSignatureGenericsTypeWriter) {
        this.classSignatureGenericsTypeWriter = classSignatureGenericsTypeWriter;
    }

    public void setDupClassInfoWriter(Writer dupClassInfoWriter) {
        this.dupClassInfoWriter = dupClassInfoWriter;
    }

    public void setDupMethodInfoWriter(Writer dupMethodInfoWriter) {
        this.dupMethodInfoWriter = dupMethodInfoWriter;
    }

    public void setDupFieldInfoWriter(Writer dupFieldInfoWriter) {
        this.dupFieldInfoWriter = dupFieldInfoWriter;
    }

    public void setEnumInitArgFieldWriter(Writer enumInitArgFieldWriter) {
        this.enumInitArgFieldWriter = enumInitArgFieldWriter;
    }

    public void setEnumInitAssignInfoWriter(Writer enumInitAssignInfoWriter) {
        this.enumInitAssignInfoWriter = enumInitAssignInfoWriter;
    }

    public void setExtendsImplWriter(Writer extendsImplWriter) {
        this.extendsImplWriter = extendsImplWriter;
    }

    public void setFieldAnnotationWriter(Writer fieldAnnotationWriter) {
        this.fieldAnnotationWriter = fieldAnnotationWriter;
    }

    public void setFieldGenericsTypeWriter(Writer fieldGenericsTypeWriter) {
        this.fieldGenericsTypeWriter = fieldGenericsTypeWriter;
    }

    public void setFieldInfoWriter(Writer fieldInfoWriter) {
        this.fieldInfoWriter = fieldInfoWriter;
    }

    public void setFieldRelationshipWriter(Writer fieldRelationshipWriter) {
        this.fieldRelationshipWriter = fieldRelationshipWriter;
    }

    public void setFieldUsageOtherWriter(Writer fieldUsageOtherWriter) {
        this.fieldUsageOtherWriter = fieldUsageOtherWriter;
    }

    public void setGetMethodWriter(Writer getMethodWriter) {
        this.getMethodWriter = getMethodWriter;
    }

    public void setInnerClassWriter(Writer innerClassWriter) {
        this.innerClassWriter = innerClassWriter;
    }

    public void setLambdaMethodInfoWriter(Writer lambdaMethodInfoWriter) {
        this.lambdaMethodInfoWriter = lambdaMethodInfoWriter;
    }

    public void setMethodAnnotationWriter(Writer methodAnnotationWriter) {
        this.methodAnnotationWriter = methodAnnotationWriter;
    }

    public void setMethodArgAnnotationWriter(Writer methodArgAnnotationWriter) {
        this.methodArgAnnotationWriter = methodArgAnnotationWriter;
    }

    public void setMethodArgGenericsTypeWriter(Writer methodArgGenericsTypeWriter) {
        this.methodArgGenericsTypeWriter = methodArgGenericsTypeWriter;
    }

    public void setMethodArgumentWriter(Writer methodArgumentWriter) {
        this.methodArgumentWriter = methodArgumentWriter;
    }

    public void setMethodCallInfoWriter(Writer methodCallInfoWriter) {
        this.methodCallInfoWriter = methodCallInfoWriter;
    }

    public void setMethodCallMethodCallReturnWriter(Writer methodCallMethodCallReturnWriter) {
        this.methodCallMethodCallReturnWriter = methodCallMethodCallReturnWriter;
    }

    public void setMethodCallStaticFieldWriter(Writer methodCallStaticFieldWriter) {
        this.methodCallStaticFieldWriter = methodCallStaticFieldWriter;
    }

    public void setMethodCallNonStaticFieldWriter(Writer methodCallNonStaticFieldWriter) {
        this.methodCallNonStaticFieldWriter = methodCallNonStaticFieldWriter;
    }

    public void setMethodCallStaticFieldMCRWriter(Writer methodCallStaticFieldMCRWriter) {
        this.methodCallStaticFieldMCRWriter = methodCallStaticFieldMCRWriter;
    }

    public void setMethodCallWriter(Writer methodCallWriter) {
        this.methodCallWriter = methodCallWriter;
    }

    public void setMethodCallRawCalleeWriter(Writer methodCallRawCalleeWriter) {
        this.methodCallRawCalleeWriter = methodCallRawCalleeWriter;
    }

    public void setMethodCatchWriter(Writer methodCatchWriter) {
        this.methodCatchWriter = methodCatchWriter;
    }

    public void setMethodFinallyWriter(Writer methodFinallyWriter) {
        this.methodFinallyWriter = methodFinallyWriter;
    }

    public void setMethodInfoWriter(Writer methodInfoWriter) {
        this.methodInfoWriter = methodInfoWriter;
    }

    public void setMethodLineNumberWriter(Writer methodLineNumberWriter) {
        this.methodLineNumberWriter = methodLineNumberWriter;
    }

    public void setMethodReturnArgSeqWriter(Writer methodReturnArgSeqWriter) {
        this.methodReturnArgSeqWriter = methodReturnArgSeqWriter;
    }

    public void setMethodReturnCallIdWriter(Writer methodReturnCallIdWriter) {
        this.methodReturnCallIdWriter = methodReturnCallIdWriter;
    }

    public void setMethodReturnConstValueWriter(Writer methodReturnConstValueWriter) {
        this.methodReturnConstValueWriter = methodReturnConstValueWriter;
    }

    public void setMethodReturnFieldInfoWriter(Writer methodReturnFieldInfoWriter) {
        this.methodReturnFieldInfoWriter = methodReturnFieldInfoWriter;
    }

    public void setMethodReturnGenericsTypeWriter(Writer methodReturnGenericsTypeWriter) {
        this.methodReturnGenericsTypeWriter = methodReturnGenericsTypeWriter;
    }

    public void setMethodThrowWriter(Writer methodThrowWriter) {
        this.methodThrowWriter = methodThrowWriter;
    }

    public void setPackageInfoWriter(Writer packageInfoWriter) {
        this.packageInfoWriter = packageInfoWriter;
    }

    public void setSetMethodWriter(Writer setMethodWriter) {
        this.setMethodWriter = setMethodWriter;
    }

    public void setStaticFinalFieldMethodCallIdWriter(Writer staticFinalFieldMethodCallIdWriter) {
        this.staticFinalFieldMethodCallIdWriter = staticFinalFieldMethodCallIdWriter;
    }

    public void setLogMethodSpendTimeWriter(WriterSupportSkip logMethodSpendTimeWriter) {
        this.logMethodSpendTimeWriter = logMethodSpendTimeWriter;
    }

    public void setCallIdCounter(JavaCG2Counter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setClassNumCounter(JavaCG2Counter classNumCounter) {
        this.classNumCounter = classNumCounter;
    }

    public void setMethodNumCounter(JavaCG2Counter methodNumCounter) {
        this.methodNumCounter = methodNumCounter;
    }

    public void setFailCounter(JavaCG2Counter failCounter) {
        this.failCounter = failCounter;
    }

    public void setFieldRelationshipCounter(JavaCG2Counter fieldRelationshipCounter) {
        this.fieldRelationshipCounter = fieldRelationshipCounter;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
