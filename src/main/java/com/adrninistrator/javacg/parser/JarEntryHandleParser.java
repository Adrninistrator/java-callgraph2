package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.classes.ClassSignatureGenericsInfo;
import com.adrninistrator.javacg.dto.classes.InnerClassInfo;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.handler.ClassHandler;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGSignatureUtil;
import com.adrninistrator.javacg.writer.WriterSupportSkip;
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

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;

    private Writer jarInfoWriter;
    private Writer classReferenceWriter;
    private Writer methodCallWriter;
    private Writer lambdaMethodInfoWriter;
    private Writer classAnnotationWriter;
    private Writer methodAnnotationWriter;
    private Writer fieldAnnotationWriter;
    private Writer fieldInfoWriter;
    private Writer methodLineNumberWriter;
    private Writer methodCallInfoWriter;
    private Writer methodCallMethodCallReturnWriter;
    private Writer methodCallStaticFieldWriter;
    private Writer methodReturnArgSeqWriter;
    private Writer methodReturnCallIdWriter;
    private Writer classInfoWriter;
    private Writer methodInfoWriter;
    private Writer extendsImplWriter;
    private Writer classSignatureEI1Writer;
    private Writer classSignatureGenericsWriter;
    private Writer classSigExtImplGenericsWriter;
    private Writer methodArgumentWriter;
    private Writer methodArgAnnotationWriter;
    private Writer methodArgGenericsTypeWriter;
    private Writer methodReturnGenericsTypeWriter;
    private Writer methodCatchWriter;
    private Writer methodFinallyWriter;
    private Writer methodThrowWriter;
    private Writer innerClassWriter;
    private Writer getMethodWriter;
    private Writer setMethodWriter;
    private Writer fieldGenericsTypeWriter;
    private Writer fieldRelationshipWriter;
    private Writer staticFinalFieldMethodCallIdWriter;

    private WriterSupportSkip logMethodSpendTimeWriter;

    // 扩展类管理类
    private ExtensionsManager extensionsManager;

    // 已经记录过的jar序号
    private final Set<Integer> recordedJarNum = new HashSet<>();

    /*
        记录已处理过的类名
        key
            类名
        value
            class文件路径
     */
    private final Map<String, List<String>> handledClassNameMap = new HashMap<>();

    // 重复的类名，结构同上
    private final Map<String, List<String>> duplicateClassNameMap = new HashMap<>();

    private JavaCGCounter callIdCounter;
    private JavaCGCounter classNumCounter;
    private JavaCGCounter methodNumCounter;
    private JavaCGCounter failCounter;
    private JavaCGCounter fieldRelationshipCounter;

    private ClassAndJarNum classAndJarNum;

    public JarEntryHandleParser(JavaCGConfInfo javaCGConfInfo, Map<String, JarInfo> jarInfoMap) {
        super(javaCGConfInfo, jarInfoMap);
    }

    @Override
    protected boolean handleEntry(ZipInputStream zipInputStream, String jarEntryPath) throws IOException {
        // 尝试记录Jar包信息
        tryRecordJarInfo();

        if (!JavaCGFileUtil.isClassFile(jarEntryPath)) {
            // 非class文件则跳过
            return true;
        }

        JavaClass javaClass = new ClassParser(zipInputStream, jarEntryPath).parse();
        // 判断是否忽略当前类
        if (ignoreCurrentClass(javaClass.getClassName())) {
            return true;
        }
        // 处理jar包中的class文件
        return handleClassEntry(javaClass, jarEntryPath);
    }

    @Override
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryPath) throws IOException {
        // 处理Java类
        return handleJavaClass(javaClass, jarEntryPath);
    }

    // 尝试记录Jar包信息
    private void tryRecordJarInfo() throws IOException {
        int lastJarNum = lastJarInfo.getJarNum();
        if (recordedJarNum.add(lastJarNum)) {
            /*
                当前jar包未记录时
                向文件写入数据，内容为jar包信息
             */
            JavaCGFileUtil.write2FileWithTab(jarInfoWriter, lastJarInfo.getJarType(), String.valueOf(lastJarNum), lastJarInfo.getJarPath());
        }
    }

    // 处理Java类
    private boolean handleJavaClass(JavaClass javaClass, String jarEntryPath) throws IOException {
        String className = javaClass.getClassName();
        List<String> classFilePathList = handledClassNameMap.get(className);
        if (classFilePathList != null) {
            // 记录已处理过的类名
            classFilePathList.add(jarEntryPath);
            // 记录重复的类名
            duplicateClassNameMap.put(className, classFilePathList);
            logger.debug("跳过处理重复同名Class: {}", className);
            return true;
        }

        classFilePathList = new ArrayList<>();
        classFilePathList.add(jarEntryPath);
        handledClassNameMap.put(className, classFilePathList);
        logger.debug("处理Class: {}", className);

        String classJarNum = classAndJarNum.getJarNum(className);
        ClassHandler classHandler = new ClassHandler(javaClass, jarEntryPath, javaCGConfInfo, classJarNum);
        classHandler.setUseSpringBeanByAnnotationHandler(useSpringBeanByAnnotationHandler);
        classHandler.setRunnableImplClassMap(runnableImplClassMap);
        classHandler.setCallableImplClassMap(callableImplClassMap);
        classHandler.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        classHandler.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        classHandler.setThreadChildClassMap(threadChildClassMap);
        classHandler.setCallIdCounter(callIdCounter);
        classHandler.setClassReferenceWriter(classReferenceWriter);
        classHandler.setClassAnnotationWriter(classAnnotationWriter);
        classHandler.setMethodAnnotationWriter(methodAnnotationWriter);
        classHandler.setFieldAnnotationWriter(fieldAnnotationWriter);
        classHandler.setFieldInfoWriter(fieldInfoWriter);
        classHandler.setMethodLineNumberWriter(methodLineNumberWriter);
        classHandler.setMethodCallWriter(methodCallWriter);
        classHandler.setGetMethodWriter(getMethodWriter);
        classHandler.setSetMethodWriter(setMethodWriter);
        classHandler.setFieldGenericsTypeWriter(fieldGenericsTypeWriter);
        classHandler.setFieldRelationshipWriter(fieldRelationshipWriter);
        classHandler.setStaticFinalFieldMethodCallIdWriter(staticFinalFieldMethodCallIdWriter);
        classHandler.setLambdaMethodInfoWriter(lambdaMethodInfoWriter);
        classHandler.setMethodCallInfoWriter(methodCallInfoWriter);
        classHandler.setMethodCallMethodCallReturnWriter(methodCallMethodCallReturnWriter);
        classHandler.setMethodCallStaticFieldWriter(methodCallStaticFieldWriter);
        classHandler.setMethodReturnArgSeqWriter(methodReturnArgSeqWriter);
        classHandler.setMethodReturnCallIdWriter(methodReturnCallIdWriter);
        classHandler.setMethodInfoWriter(methodInfoWriter);
        classHandler.setMethodArgumentWriter(methodArgumentWriter);
        classHandler.setMethodArgAnnotationWriter(methodArgAnnotationWriter);
        classHandler.setMethodArgGenericsTypeWriter(methodArgGenericsTypeWriter);
        classHandler.setMethodReturnGenericsTypeWriter(methodReturnGenericsTypeWriter);
        classHandler.setMethodCatchWriter(methodCatchWriter);
        classHandler.setMethodFinallyWriter(methodFinallyWriter);
        classHandler.setMethodThrowWriter(methodThrowWriter);
        classHandler.setLogMethodSpendTimeWriter(logMethodSpendTimeWriter);
        classHandler.setExtensionsManager(extensionsManager);
        classHandler.setMethodNumCounter(methodNumCounter);
        classHandler.setFailCounter(failCounter);
        classHandler.setFieldRelationshipCounter(fieldRelationshipCounter);
        classHandler.setLastJarNum(lastJarInfo.getJarNum());
        classHandler.setClassAndJarNum(classAndJarNum);

        classNumCounter.addAndGet();
        int failCountBefore = failCounter.getCount();
        // 处理当前类
        boolean success = classHandler.handleClass();
        if (failCounter.getCount() > failCountBefore) {
            // 将处理失败的类保存到目录中
            saveHandleFailClass(javaClass);
        }
        if (!success) {
            return false;
        }

        String classMd5 = DigestUtils.md5Hex(javaClass.getBytes());
        // 记录类的信息
        JavaCGFileUtil.write2FileWithTab(classInfoWriter, className, String.valueOf(javaClass.getAccessFlags()), classMd5, classJarNum);

        // 记录继承及实现相关信息
        recordExtendsAndImplInfo(javaClass, className);

        // 处理类的签名
        handleClassSignature(javaClass, className);

        // 处理内部类信息
        handleInnerClass(javaClass);
        return true;
    }

    // 将处理失败的类保存到目录中
    private void saveHandleFailClass(JavaClass javaClass) {
        String saveClassFilePath = javaCGConfInfo.getUsedOutputDirPath() + JavaCGConstants.DIR_FAIL_CLASSES + File.separator +
                javaClass.getClassName() + JavaCGConstants.EXT_CLASS;
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
        if (!JavaCGClassMethodUtil.isObjectClass(superClassName)) {
            // 仅处理父类非Object类的情况
            JavaCGFileUtil.write2FileWithTab(extendsImplWriter,
                    className,
                    accessFlagsStr,
                    JavaCGConstants.FILE_KEY_EXTENDS,
                    superClassName);
        }

        for (String interfaceName : javaClass.getInterfaceNames()) {
            // 接口不会是Object类，不需要判断
            JavaCGFileUtil.write2FileWithTab(extendsImplWriter,
                    className,
                    accessFlagsStr,
                    JavaCGConstants.FILE_KEY_IMPLEMENTS,
                    interfaceName);
        }
    }

    // 处理类的签名
    private void handleClassSignature(JavaClass javaClass, String className) throws IOException {
        if (javaClass.isAnnotation()) {
            // 若当前类为注解则不处理
            return;
        }

        Signature signature = JavaCGByteCodeUtil.getSignatureOfClass(javaClass);
        if (signature == null) {
            return;
        }

        try {
            SignatureAttribute.ClassSignature signatureAttribute = SignatureAttribute.toClassSignature(signature.getSignature());

            // 类涉及继承与实现的签名信息Map
            Map<String, ClassSignatureGenericsInfo> signatureInfoMap = new HashMap<>();
            SignatureAttribute.TypeParameter[] params = signatureAttribute.getParameters();
            if (ArrayUtils.isNotEmpty(params)) {
                // 处理类签名中的泛型类型
                int seq = -1;
                for (SignatureAttribute.TypeParameter param : params) {
                    seq++;
                    String paramName = param.getName();
                    SignatureAttribute.ObjectType paramClassBound = param.getClassBound();
                    if (paramClassBound != null) {
                        // 泛型类型继承了特定类的处理
                        String extendsClassName = paramClassBound.jvmTypeName();
                        JavaCGFileUtil.write2FileWithTab(classSignatureGenericsWriter, className, String.valueOf(seq), paramName, extendsClassName);
                        signatureInfoMap.put(paramName, new ClassSignatureGenericsInfo(extendsClassName, seq));
                        continue;
                    }
                    SignatureAttribute.ObjectType[] paramInterfaceBounds = param.getInterfaceBound();
                    if (ArrayUtils.isNotEmpty(paramInterfaceBounds)) {
                        // 泛型类型继承了特定接口的处理
                        String extendsClassName = paramInterfaceBounds[0].jvmTypeName();
                        JavaCGFileUtil.write2FileWithTab(classSignatureGenericsWriter, className, String.valueOf(seq), paramName, extendsClassName);
                        signatureInfoMap.put(paramName, new ClassSignatureGenericsInfo(extendsClassName, seq));
                    }
                }
            }

            // 处理父类相关的签名
            SignatureAttribute.ClassType superClassType = signatureAttribute.getSuperClass();
            if (superClassType != null) {
                String superClassName = JavaCGSignatureUtil.getClassName(superClassType);
                // 记录类签名中的参数信息
                recordSignatureArgumentInfo(true, className, JavaCGConstants.FILE_KEY_EXTENDS, superClassName, superClassType, signatureInfoMap);
            }

            // 处理接口相关的签名
            SignatureAttribute.ClassType[] interfaceClassTypes = signatureAttribute.getInterfaces();
            if (interfaceClassTypes != null) {
                for (SignatureAttribute.ClassType interfaceClassType : interfaceClassTypes) {
                    String interfaceClassName = JavaCGSignatureUtil.getClassName(interfaceClassType);
                    // 记录类签名中的参数信息
                    recordSignatureArgumentInfo(false, className, JavaCGConstants.FILE_KEY_IMPLEMENTS, interfaceClassName, interfaceClassType, signatureInfoMap);
                }
            }
        } catch (BadBytecode e) {
            logger.error("处理类的签名出现异常 {} ", className, e);
        }
    }

    // 处理内部类信息
    private void handleInnerClass(JavaClass javaClass) throws IOException {
        // 获取类中的内部类信息
        List<InnerClassInfo> innerClassInfoList = JavaCGByteCodeUtil.getInnerClassInfo(javaClass);
        for (InnerClassInfo innerClassInfo : innerClassInfoList) {
            JavaCGFileUtil.write2FileWithTab(innerClassWriter, innerClassInfo.getInnerClassName(), innerClassInfo.getOuterClassName(),
                    JavaCGYesNoEnum.parseStrValue(innerClassInfo.isAnonymousClass()));
        }
    }

    /**
     * 记录类签名中的参数信息
     *
     * @param extendsOrImpl
     * @param className
     * @param type
     * @param superOrInterfaceName
     * @param classType
     * @param signatureInfoMap
     */
    private void recordSignatureArgumentInfo(boolean extendsOrImpl, String className, String type, String superOrInterfaceName, SignatureAttribute.ClassType classType,
                                             Map<String, ClassSignatureGenericsInfo> signatureInfoMap) throws IOException {
        SignatureAttribute.TypeArgument[] arguments = classType.getTypeArguments();
        if (arguments == null) {
            return;
        }

        int seq = 0;
        for (SignatureAttribute.TypeArgument typeArgument : arguments) {
            SignatureAttribute.ObjectType objectType = typeArgument.getType();
            String genericsName = null;
            if (objectType instanceof SignatureAttribute.ClassType) {
                SignatureAttribute.ClassType argumentClassType = (SignatureAttribute.ClassType) objectType;
                JavaCGFileUtil.write2FileWithTab(classSignatureEI1Writer, className, type, superOrInterfaceName, String.valueOf(seq),
                        JavaCGSignatureUtil.getClassName(argumentClassType), "");
            } else if (objectType instanceof SignatureAttribute.TypeVariable) {
                SignatureAttribute.TypeVariable typeVariable = (SignatureAttribute.TypeVariable) objectType;
                JavaCGFileUtil.write2FileWithTab(classSignatureEI1Writer, className, type, superOrInterfaceName, String.valueOf(seq), "", typeVariable.getName());
                genericsName = typeVariable.getName();
            } else if (objectType instanceof SignatureAttribute.ArrayType) {
                SignatureAttribute.ArrayType argumentArrayType = (SignatureAttribute.ArrayType) objectType;
                JavaCGFileUtil.write2FileWithTab(classSignatureEI1Writer, className, type, superOrInterfaceName, String.valueOf(seq), argumentArrayType.toString(), "");
                genericsName = argumentArrayType.toString();
            }
            if (StringUtils.isNotBlank(genericsName)) {
                ClassSignatureGenericsInfo genericsInfo = signatureInfoMap.get(genericsName);
                if (genericsInfo != null) {
                    // 记录当前类/接口继承或实现时与父类或接口相同的泛型名称
                    JavaCGFileUtil.write2FileWithTab(classSigExtImplGenericsWriter, className, genericsName, String.valueOf(genericsInfo.getSeq()), (extendsOrImpl ?
                                    JavaCGConstants.FILE_KEY_EXTENDS : JavaCGConstants.FILE_KEY_IMPLEMENTS), superOrInterfaceName, genericsInfo.getExtendsClassName(),
                            String.valueOf(seq));
                }
            }
            seq++;
        }
    }

    //
    public Map<String, List<String>> getDuplicateClassNameMap() {
        return duplicateClassNameMap;
    }

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

    public void setJarInfoWriter(Writer jarInfoWriter) {
        this.jarInfoWriter = jarInfoWriter;
    }

    public void setClassReferenceWriter(Writer classReferenceWriter) {
        this.classReferenceWriter = classReferenceWriter;
    }

    public void setMethodCallWriter(Writer methodCallWriter) {
        this.methodCallWriter = methodCallWriter;
    }

    public void setLambdaMethodInfoWriter(Writer lambdaMethodInfoWriter) {
        this.lambdaMethodInfoWriter = lambdaMethodInfoWriter;
    }

    public void setClassAnnotationWriter(Writer classAnnotationWriter) {
        this.classAnnotationWriter = classAnnotationWriter;
    }

    public void setMethodAnnotationWriter(Writer methodAnnotationWriter) {
        this.methodAnnotationWriter = methodAnnotationWriter;
    }

    public void setFieldAnnotationWriter(Writer fieldAnnotationWriter) {
        this.fieldAnnotationWriter = fieldAnnotationWriter;
    }

    public void setFieldInfoWriter(Writer fieldInfoWriter) {
        this.fieldInfoWriter = fieldInfoWriter;
    }

    public void setMethodLineNumberWriter(Writer methodLineNumberWriter) {
        this.methodLineNumberWriter = methodLineNumberWriter;
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

    public void setMethodReturnArgSeqWriter(Writer methodReturnArgSeqWriter) {
        this.methodReturnArgSeqWriter = methodReturnArgSeqWriter;
    }

    public void setMethodReturnCallIdWriter(Writer methodReturnCallIdWriter) {
        this.methodReturnCallIdWriter = methodReturnCallIdWriter;
    }

    public void setClassInfoWriter(Writer classInfoWriter) {
        this.classInfoWriter = classInfoWriter;
    }

    public void setMethodInfoWriter(Writer methodInfoWriter) {
        this.methodInfoWriter = methodInfoWriter;
    }

    public void setExtendsImplWriter(Writer extendsImplWriter) {
        this.extendsImplWriter = extendsImplWriter;
    }

    public void setClassSignatureEI1Writer(Writer classSignatureEI1Writer) {
        this.classSignatureEI1Writer = classSignatureEI1Writer;
    }

    public void setClassSignatureGenericsWriter(Writer classSignatureGenericsWriter) {
        this.classSignatureGenericsWriter = classSignatureGenericsWriter;
    }

    public void setClassSigExtImplGenericsWriter(Writer classSigExtImplGenericsWriter) {
        this.classSigExtImplGenericsWriter = classSigExtImplGenericsWriter;
    }

    public void setMethodArgumentWriter(Writer methodArgumentWriter) {
        this.methodArgumentWriter = methodArgumentWriter;
    }

    public void setMethodArgAnnotationWriter(Writer methodArgAnnotationWriter) {
        this.methodArgAnnotationWriter = methodArgAnnotationWriter;
    }

    public void setMethodArgGenericsTypeWriter(Writer methodArgGenericsTypeWriter) {
        this.methodArgGenericsTypeWriter = methodArgGenericsTypeWriter;
    }

    public void setMethodReturnGenericsTypeWriter(Writer methodReturnGenericsTypeWriter) {
        this.methodReturnGenericsTypeWriter = methodReturnGenericsTypeWriter;
    }

    public void setMethodCatchWriter(Writer methodCatchWriter) {
        this.methodCatchWriter = methodCatchWriter;
    }

    public void setMethodFinallyWriter(Writer methodFinallyWriter) {
        this.methodFinallyWriter = methodFinallyWriter;
    }

    public void setMethodThrowWriter(Writer methodThrowWriter) {
        this.methodThrowWriter = methodThrowWriter;
    }

    public void setInnerClassWriter(Writer innerClassWriter) {
        this.innerClassWriter = innerClassWriter;
    }

    public void setGetMethodWriter(Writer getMethodWriter) {
        this.getMethodWriter = getMethodWriter;
    }

    public void setSetMethodWriter(Writer setMethodWriter) {
        this.setMethodWriter = setMethodWriter;
    }

    public void setFieldGenericsTypeWriter(Writer fieldGenericsTypeWriter) {
        this.fieldGenericsTypeWriter = fieldGenericsTypeWriter;
    }

    public void setFieldRelationshipWriter(Writer fieldRelationshipWriter) {
        this.fieldRelationshipWriter = fieldRelationshipWriter;
    }

    public void setStaticFinalFieldMethodCallIdWriter(Writer staticFinalFieldMethodCallIdWriter) {
        this.staticFinalFieldMethodCallIdWriter = staticFinalFieldMethodCallIdWriter;
    }

    public void setLogMethodSpendTimeWriter(WriterSupportSkip logMethodSpendTimeWriter) {
        this.logMethodSpendTimeWriter = logMethodSpendTimeWriter;
    }

    public void setExtensionsManager(ExtensionsManager extensionsManager) {
        this.extensionsManager = extensionsManager;
    }

    public void setCallIdCounter(JavaCGCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setClassNumCounter(JavaCGCounter classNumCounter) {
        this.classNumCounter = classNumCounter;
    }

    public void setMethodNumCounter(JavaCGCounter methodNumCounter) {
        this.methodNumCounter = methodNumCounter;
    }

    public void setFailCounter(JavaCGCounter failCounter) {
        this.failCounter = failCounter;
    }

    public void setFieldRelationshipCounter(JavaCGCounter fieldRelationshipCounter) {
        this.fieldRelationshipCounter = fieldRelationshipCounter;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
