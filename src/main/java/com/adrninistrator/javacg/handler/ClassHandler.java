package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg.dto.instruction.InvokeInstructionPosAndCallee;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.extensions.annotationattributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGAnnotationUtil;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGSignatureUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import com.adrninistrator.javacg.writer.WriterSupportSkip;
import copy.javassist.bytecode.BadBytecode;
import copy.javassist.bytecode.SignatureAttribute;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.ClassFormatException;
import org.apache.bcel.classfile.Code;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LocalVariable;
import org.apache.bcel.classfile.LocalVariableTable;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.MethodParameter;
import org.apache.bcel.classfile.MethodParameters;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
 * @description: 对类进行处理
 */
public class ClassHandler {

    private static final Logger logger = LoggerFactory.getLogger(ClassHandler.class);

    private final JavaClass javaClass;

    private final String classFileName;

    private final ConstantPoolGen cpg;

    private final Set<String> handledMethodNameAndArgs;

    private final String className;

    private final JavaCGConfInfo javaCGConfInfo;

    private final String classJarNum;

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;

    private ExtensionsManager extensionsManager;

    private JavaCGCounter callIdCounter;
    private JavaCGCounter methodNumCounter;
    private JavaCGCounter failCounter;
    private JavaCGCounter fieldRelationshipCounter;

    private Writer classNameWriter;
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
    private Writer methodInfoWriter;
    private Writer methodArgumentWriter;
    private Writer methodArgAnnotationWriter;
    private Writer methodArgGenericsTypeWriter;
    private Writer methodReturnGenericsTypeWriter;
    private Writer methodCatchWriter;
    private Writer methodFinallyWriter;
    private Writer methodThrowWriter;
    private Writer getMethodWriter;
    private Writer setMethodWriter;
    private Writer fieldGenericsTypeWriter;
    private Writer fieldRelationshipWriter;
    private Writer staticFinalFieldMethodCallIdWriter;
    private WriterSupportSkip logMethodSpendTimeWriter;

    private AnnotationAttributesFormatterInterface annotationAttributesFormatter;

    private int lastJarNum;

    private ClassAndJarNum classAndJarNum;

    // 非静态字段字段所有可能的类型
    private FieldPossibleTypes nonStaticFieldPossibleTypes;

    // 当前类已记录过的set方法名称（get方法没有参数，只会有一个）
    private Set<String> recordedSetMethodSet;

    public ClassHandler(JavaClass javaClass, String classFileName, JavaCGConfInfo javaCGConfInfo, String classJarNum) {
        this.javaClass = javaClass;
        this.classFileName = classFileName;
        this.javaCGConfInfo = javaCGConfInfo;
        this.classJarNum = classJarNum;

        className = javaClass.getClassName();
        cpg = new ConstantPoolGen(javaClass.getConstantPool());
        handledMethodNameAndArgs = new HashSet<>();

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            recordedSetMethodSet = new HashSet<>();
        }
        if (javaCGConfInfo.isFirstParseInitMethodType()) {
            nonStaticFieldPossibleTypes = new FieldPossibleTypes();
        }
    }

    // 记录类之间引用关系
    private void recordReferencedClass() throws IOException {
        // 处理引用类
        ConstantPool constantPool = javaClass.getConstantPool();

        Set<String> referencedClassSet = new HashSet<>();

        for (int i = 0; i < constantPool.getLength(); i++) {
            Constant constant = null;
            try {
                constant = constantPool.getConstant(i);
            } catch (ClassFormatException e) {
                // 正常情况，不需要处理
            }
            if (constant == null || constant.getTag() != Const.CONSTANT_Class) {
                continue;
            }

            String referencedClass = constantPool.constantToString(constant);
            if (!JavaCGClassMethodUtil.isObjectClass(referencedClass)) {
                // 只处理非Object类的引用，去除类名中的数组形式
                referencedClass = JavaCGByteCodeUtil.removeArrayInClassName(referencedClass);
                referencedClassSet.add(referencedClass);
            }
        }

        // 首先写入当前类的类名
        JavaCGFileUtil.write2FileWithTab(classNameWriter, JavaCGConstants.FLAG_HASHTAG);
        JavaCGFileUtil.write2FileWithTab(classNameWriter, className);

        List<String> referencedClassList = new ArrayList<>(referencedClassSet);
        Collections.sort(referencedClassList);
        // 写入其他被类的类名
        for (String referencedClass : referencedClassList) {
            if (JavaCGUtil.checkSkipClass(referencedClass, javaCGConfInfo.getNeedHandlePackageSet()) ||
                    className.equals(referencedClass)) {
                continue;
            }
            JavaCGFileUtil.write2FileWithTab(classNameWriter, referencedClass);
        }
    }

    public boolean handleClass() throws IOException {
        // 记录类之间引用关系
        recordReferencedClass();

        if (!javaClass.isAnnotation()) {
            // 不是注解类型的类
            // 记录类上的注解信息
            JavaCGAnnotationUtil.writeAnnotationInfo(classAnnotationWriter,
                    javaClass.getAnnotationEntries(),
                    annotationAttributesFormatter,
                    className);
        }

        if (javaCGConfInfo.isFirstParseInitMethodType()) {
            for (Method method : javaClass.getMethods()) {
                // 解析构造函数
                if (JavaCGClassMethodUtil.isInitMethod(method.getName()) &&
                        !parseInitMethod(method)) {
                    return false;
                }
            }
        }

        /*
            记录当前类非静态的字段名称及类型Map
            key     字段名称
            value   字段类型
         */
        Map<String, String> nonStaticFieldNameTypeMap = new HashMap<>();

        /*
            记录当前类非静态的字段名称及集合类型中的泛型类型Map
            key     字段名称
            value   集合类型中的泛型类型
         */
        Map<String, List<String>> nonStaticFieldNameGenericsTypeMap = new HashMap<>();

        /*
            记录当前类的static、final字段名称及初始化方法call_id的Map
            key     当前类的static、final字段名称
            value   对应的方法调用指令位置及被调用方法列表
         */
        Map<String, List<InvokeInstructionPosAndCallee>> sfFieldInvokeInstructionMap = new HashMap<>();

        /*
            记录当前类的static、final字段名称及对应的类型的Map
            key     当前类的static、final字段名称
            value   当前类的static、final字段类型
         */
        Map<String, String> staticFinalFieldNameTypeMap = new HashMap<>();

        // dto的非静态字段集合中涉及的泛型类型已记录的字段名称Set
        Set<String> recordedFieldWithGenericsTypeSet = new HashSet<>();

        // 处理字段
        for (Field field : javaClass.getFields()) {
            handleField(field, nonStaticFieldNameTypeMap, nonStaticFieldNameGenericsTypeMap, sfFieldInvokeInstructionMap, staticFinalFieldNameTypeMap);
        }

        // 处理方法
        for (Method method : javaClass.getMethods()) {
            if (!handleMethod(method, nonStaticFieldNameTypeMap, nonStaticFieldNameGenericsTypeMap, sfFieldInvokeInstructionMap, staticFinalFieldNameTypeMap,
                    recordedFieldWithGenericsTypeSet)) {
                return false;
            }
        }

        return true;
    }

    // 解析构造函数
    private boolean parseInitMethod(Method method) {
        MethodGen mg = new MethodGen(method, className, cpg);
        String callerFullMethod = JavaCGClassMethodUtil.formatFullMethod(className, method.getName(), method.getArgumentTypes());
        MethodHandler4TypeAndValue methodHandler4TypeAndValue = new MethodHandler4TypeAndValue(method, mg, javaClass, callerFullMethod, javaCGConfInfo);
        methodHandler4TypeAndValue.setFailCounter(failCounter);
        methodHandler4TypeAndValue.setRecordFieldPossibleTypeFlag(true);
        methodHandler4TypeAndValue.setUseFieldPossibleTypeFlag(false);
        methodHandler4TypeAndValue.setAnalyseFieldRelationshipFlag(false);
        methodHandler4TypeAndValue.setOnlyAnalyseReturnTypeFlag(false);
        methodHandler4TypeAndValue.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);

        return methodHandler4TypeAndValue.handleMethod();
    }

    // 处理字段
    private void handleField(Field field,
                             Map<String, String> nonStaticFieldNameTypeMap,
                             Map<String, List<String>> nonStaticFieldNameGenericsTypeMap,
                             Map<String, List<InvokeInstructionPosAndCallee>> sfFieldInvokeInstructionMap,
                             Map<String, String> staticFinalFieldNameTypeMap) throws IOException {
        String fieldName = field.getName();
        String fieldTypeStr = field.getType().toString();
        String fieldModifiers = JavaCGByteCodeUtil.getModifiersString(field.getAccessFlags());
        String primitiveType = JavaCGYesNoEnum.parseStrValue(JavaCGConstantTypeEnum.isPrimitiveType(fieldTypeStr));
        String staticFlag = JavaCGYesNoEnum.parseStrValue(field.isStatic());
        String finalFlag = JavaCGYesNoEnum.parseStrValue(field.isFinal());
        // 记录字段信息
        JavaCGFileUtil.write2FileWithTab(fieldInfoWriter, className, fieldName, fieldTypeStr, fieldModifiers, primitiveType, staticFlag, finalFlag);

        // 记录字段上的注解信息
        JavaCGAnnotationUtil.writeAnnotationInfo(fieldAnnotationWriter, field.getAnnotationEntries(), annotationAttributesFormatter, className, fieldName);

        if (javaCGConfInfo.isParseMethodCallTypeValue() && field.isStatic() && field.isFinal()) {
            // 处理static、final字段
            sfFieldInvokeInstructionMap.put(fieldName, new ArrayList<>(1));
            staticFinalFieldNameTypeMap.put(fieldName, field.getType().toString());
        }

        if (javaCGConfInfo.isAnalyseFieldRelationship() && !field.isStatic()) {
            // 需要分析dto的字段之间的关联关系，处理非静态字段
            try {
                String fieldGenericSignature = field.getGenericSignature();
                if (fieldGenericSignature != null) {
                    List<String> fieldGenericsTypeList = new ArrayList<>();
                    SignatureAttribute.ObjectType fieldType = SignatureAttribute.toFieldSignature(fieldGenericSignature);
                    // 解析字段定义中的泛型类型
                    JavaCGSignatureUtil.parseTypeDefineGenericsType(fieldType, true, fieldGenericsTypeList);
                    if (!fieldGenericsTypeList.isEmpty()) {
                        nonStaticFieldNameGenericsTypeMap.put(fieldName, fieldGenericsTypeList);
                    }
                }
            } catch (BadBytecode e) {
                logger.error("处理字段出现异常 {} {}", className, fieldName, e);
            }

            nonStaticFieldNameTypeMap.put(fieldName, field.getType().toString());
        }
    }

    /**
     * 处理方法
     *
     * @param method
     * @param nonStaticFieldNameTypeMap
     * @param nonStaticFieldNameGenericsTypeMap
     * @param sfFieldInvokeInstructionMap
     * @param staticFinalFieldNameTypeMap
     * @param recordedFieldWithGenericsTypeSet
     * @return false: 处理失败 true: 处理成功
     */
    private boolean handleMethod(Method method,
                                 Map<String, String> nonStaticFieldNameTypeMap,
                                 Map<String, List<String>> nonStaticFieldNameGenericsTypeMap,
                                 Map<String, List<InvokeInstructionPosAndCallee>> sfFieldInvokeInstructionMap,
                                 Map<String, String> staticFinalFieldNameTypeMap,
                                 Set<String> recordedFieldWithGenericsTypeSet) throws IOException {
        methodNumCounter.addAndGet();

        // 是否出现方法名+参数类型均相同的方法标记
        boolean existsSameMethodNameAndArgs = false;

        // 生成格式化后的方法参数
        String methodArgTypes = JavaCGClassMethodUtil.getArgTypeStr(method.getArgumentTypes());
        // 生成格式化后的完整方法
        String fullMethod = JavaCGClassMethodUtil.formatFullMethod(className, method.getName(), methodArgTypes);

        long startTime = 0;
        if (javaCGConfInfo.isLogMethodSpendTime()) {
            startTime = System.currentTimeMillis();
            // 记录当前处理的类名、方法
            logMethodSpendTimeWriter.write(classFileName + JavaCGConstants.FLAG_TAB + fullMethod);
        }

        logger.debug("处理Method: {}", fullMethod);

        // 返回类型
        String returnType = method.getReturnType().toString();
        String methodNameAndArgs = method.getName() + methodArgTypes;
        if (handledMethodNameAndArgs.add(methodNameAndArgs)) {
            // 获取方法指令的HASH
            Code code = method.getCode();
            String methodHash = "";
            if (code != null) {
                String methodCode = Utility.codeToString(code.getCode(), method.getConstantPool(), 0, -1, false);
                methodHash = DigestUtils.md5Hex(methodCode);
            }

            // 记录方法的信息
            JavaCGFileUtil.write2FileWithTab(methodInfoWriter, fullMethod, String.valueOf(method.getAccessFlags()), returnType, methodHash, classJarNum);
        } else {
            // 出现方法名+参数类型均相同的方法
            existsSameMethodNameAndArgs = true;
            if (!JavaCGByteCodeUtil.isBridgeFlag(method.getAccessFlags())) {
                logger.error("出现方法名+参数类型均相同的方法，但方法没有ACC_BRIDGE标志，与预期不符 {} {}", className, methodNameAndArgs);
            }
        }

        String methodGenericSignature = method.getGenericSignature();
        if (methodGenericSignature != null) {
            try {
                SignatureAttribute.MethodSignature methodSignature = SignatureAttribute.toMethodSignature(methodGenericSignature);
                // 记录方法返回泛型类型
                recordMethodReturnGenericsType(fullMethod, methodSignature);

                // 记录方法参数中泛型类型
                recordMethodArgsGenericsType(fullMethod, methodSignature);
            } catch (BadBytecode e) {
                logger.error("处理方法签名出现异常 {}", fullMethod, e);
            }
        }

        MethodGen mg = new MethodGen(method, className, cpg);
        if (!existsSameMethodNameAndArgs) {
            /*
                对于方法名+参数类型相同的方法，不处理方法参数
            */
            recordMethodArgument(fullMethod, mg);
        }

        // 处理方法调用
        MethodHandler4Invoke methodHandler4Invoke = new MethodHandler4Invoke(method,
                mg,
                javaClass,
                javaCGConfInfo,
                methodArgTypes,
                fullMethod,
                useSpringBeanByAnnotationHandler,
                callIdCounter);
        methodHandler4Invoke.setFailCounter(failCounter);
        methodHandler4Invoke.setRunnableImplClassMap(runnableImplClassMap);
        methodHandler4Invoke.setCallableImplClassMap(callableImplClassMap);
        methodHandler4Invoke.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        methodHandler4Invoke.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        methodHandler4Invoke.setThreadChildClassMap(threadChildClassMap);
        methodHandler4Invoke.setExtensionsManager(extensionsManager);
        methodHandler4Invoke.setMethodCallWriter(methodCallWriter);
        methodHandler4Invoke.setLambdaMethodInfoWriter(lambdaMethodInfoWriter);
        methodHandler4Invoke.setMethodAnnotationWriter(methodAnnotationWriter);
        methodHandler4Invoke.setMethodArgAnnotationWriter(methodArgAnnotationWriter);
        methodHandler4Invoke.setMethodLineNumberWriter(methodLineNumberWriter);
        methodHandler4Invoke.setLastJarNum(lastJarNum);
        methodHandler4Invoke.setExistsSameMethodNameAndArgs(existsSameMethodNameAndArgs);
        methodHandler4Invoke.setClassAndJarNum(classAndJarNum);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            methodHandler4Invoke.setParseMethodCallTypeValueFlag(true);
            methodHandler4Invoke.setMethodCallInfoWriter(methodCallInfoWriter);
            methodHandler4Invoke.setMethodCallMethodCallReturnWriter(methodCallMethodCallReturnWriter);
            methodHandler4Invoke.setMethodCallStaticFieldWriter(methodCallStaticFieldWriter);
            methodHandler4Invoke.setMethodReturnArgSeqWriter(methodReturnArgSeqWriter);
            methodHandler4Invoke.setMethodReturnCallIdWriter(methodReturnCallIdWriter);
            methodHandler4Invoke.setMethodCatchWriter(methodCatchWriter);
            methodHandler4Invoke.setMethodFinallyWriter(methodFinallyWriter);
            methodHandler4Invoke.setMethodThrowWriter(methodThrowWriter);
            methodHandler4Invoke.setStaticFinalFieldMethodCallIdWriter(staticFinalFieldMethodCallIdWriter);
            methodHandler4Invoke.setGetMethodWriter(getMethodWriter);
            methodHandler4Invoke.setSetMethodWriter(setMethodWriter);
            methodHandler4Invoke.setFieldGenericsTypeWriter(fieldGenericsTypeWriter);
            methodHandler4Invoke.setRecordedSetMethodSet(recordedSetMethodSet);
            methodHandler4Invoke.setNonStaticFieldNameTypeMap(nonStaticFieldNameTypeMap);
            methodHandler4Invoke.setNonStaticFieldNameGenericsTypeMap(nonStaticFieldNameGenericsTypeMap);
            methodHandler4Invoke.setRecordedFieldWithGenericsTypeSet(recordedFieldWithGenericsTypeSet);
            if (JavaCGCommonNameConstants.METHOD_NAME_CLINIT.equals(method.getName())) {
                // 当前方法为静态代码块
                methodHandler4Invoke.setInClinitMethod(true);
                methodHandler4Invoke.setSfFieldInvokeInstructionMap(sfFieldInvokeInstructionMap);
                methodHandler4Invoke.setStaticFinalFieldNameTypeMap(staticFinalFieldNameTypeMap);
            }
            if (javaCGConfInfo.isFirstParseInitMethodType()) {
                methodHandler4Invoke.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);
            }
            if (javaCGConfInfo.isAnalyseFieldRelationship()) {
                methodHandler4Invoke.setFieldRelationshipWriter(fieldRelationshipWriter);
                methodHandler4Invoke.setFieldRelationshipCounter(fieldRelationshipCounter);
            }
        }

        boolean success = methodHandler4Invoke.handleMethod();
        if (javaCGConfInfo.isLogMethodSpendTime()) {
            long spendTime = System.currentTimeMillis() - startTime;
            // 记录方法处理耗时
            logMethodSpendTimeWriter.write(JavaCGConstants.FLAG_TAB + spendTime + JavaCGConstants.NEW_LINE);
        }
        return success;
    }

    // 记录方法返回泛型类型
    private void recordMethodReturnGenericsType(String fullMethod, SignatureAttribute.MethodSignature methodSignature) {
        try {
            SignatureAttribute.Type returnType = methodSignature.getReturnType();
            if (!(returnType instanceof SignatureAttribute.ClassType)) {
                return;
            }

            SignatureAttribute.ClassType classType = (SignatureAttribute.ClassType) returnType;
            List<String> methodReturnGenericsTypeList = new ArrayList<>();
            // 解析方法返回中泛型类型，外层处理
            JavaCGSignatureUtil.parseTypeDefineGenericsType(classType, true, methodReturnGenericsTypeList);
            if (methodReturnGenericsTypeList.isEmpty()) {
                // 未获取到方法返回中泛型类型
                return;
            }

            // 记录返回类型，序号固定为0
            JavaCGFileUtil.write2FileWithTab(methodReturnGenericsTypeWriter, fullMethod, JavaCGConstants.FILE_KEY_CLASS_TYPE, "0",
                    JavaCGSignatureUtil.getClassName(classType));

            // 获取到方法返回中泛型类型，记录
            for (int i = 0; i < methodReturnGenericsTypeList.size(); i++) {
                JavaCGFileUtil.write2FileWithTab(methodReturnGenericsTypeWriter, fullMethod, JavaCGConstants.FILE_KEY_GENERICS_TYPE, String.valueOf(i),
                        methodReturnGenericsTypeList.get(i));
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 记录方法参数中泛型类型
    private void recordMethodArgsGenericsType(String fullMethod, SignatureAttribute.MethodSignature methodSignature) throws IOException {
        SignatureAttribute.Type[] parameterTypes = methodSignature.getParameterTypes();
        if (ArrayUtils.isEmpty(parameterTypes)) {
            return;
        }
        for (int i = 0; i < parameterTypes.length; i++) {
            SignatureAttribute.Type argType = parameterTypes[i];
            if (!(argType instanceof SignatureAttribute.ClassType)) {
                continue;
            }

            SignatureAttribute.ClassType classType = (SignatureAttribute.ClassType) argType;
            List<String> methodArgsGenericsTypeList = new ArrayList<>();
            // 解析方法参数中泛型类型，外层处理
            JavaCGSignatureUtil.parseTypeDefineGenericsType(classType, true, methodArgsGenericsTypeList);
            if (methodArgsGenericsTypeList.isEmpty()) {
                // 未获取到方法参数中泛型类型
                continue;
            }

            // 记录参数类型，序号固定为0
            JavaCGFileUtil.write2FileWithTab(methodArgGenericsTypeWriter, fullMethod, String.valueOf(i), JavaCGConstants.FILE_KEY_CLASS_TYPE, "0",
                    JavaCGSignatureUtil.getClassName(classType));

            // 获取到方法参数中泛型类型，记录
            for (int j = 0; j < methodArgsGenericsTypeList.size(); j++) {
                JavaCGFileUtil.write2FileWithTab(methodArgGenericsTypeWriter, fullMethod, String.valueOf(i), JavaCGConstants.FILE_KEY_GENERICS_TYPE,
                        String.valueOf(j), methodArgsGenericsTypeList.get(j));
            }
        }
    }

    // 处理方法参数
    private void recordMethodArgument(String fullMethod, MethodGen mg) throws IOException {
        Type[] argTypes = mg.getArgumentTypes();

        for (Attribute attribute : mg.getAttributes()) {
            if (!(attribute instanceof MethodParameters)) {
                continue;
            }
            ConstantPool constantPool = mg.getConstantPool().getConstantPool();
            MethodParameters methodParameters = (MethodParameters) attribute;
            MethodParameter[] parameters = methodParameters.getParameters();
            for (int i = 0; i < parameters.length; i++) {
                String argName = parameters[i].getParameterName(constantPool);
                JavaCGFileUtil.write2FileWithTab(methodArgumentWriter, fullMethod, String.valueOf(i), argTypes[i].toString(), argName);
            }
            return;
        }

        LocalVariableTable localVariableTable = mg.getLocalVariableTable(mg.getConstantPool());
        for (int i = 0; i < argTypes.length; i++) {
            int argIndex = JavaCGByteCodeUtil.getLocalVariableTableIndex(mg, i);
            LocalVariable localVariable = localVariableTable.getLocalVariable(argIndex, 0);
            String argName = (localVariable == null ? "" : localVariable.getName());
            JavaCGFileUtil.write2FileWithTab(methodArgumentWriter, fullMethod, String.valueOf(i), argTypes[i].toString(), argName);
        }
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

    public void setExtensionsManager(ExtensionsManager extensionsManager) {
        this.extensionsManager = extensionsManager;
        annotationAttributesFormatter = extensionsManager.getAnnotationAttributesFormatter();
    }

    public void setCallIdCounter(JavaCGCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setClassNameWriter(Writer classNameWriter) {
        this.classNameWriter = classNameWriter;
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

    public void setMethodInfoWriter(Writer methodInfoWriter) {
        this.methodInfoWriter = methodInfoWriter;
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

    public void setMethodNumCounter(JavaCGCounter methodNumCounter) {
        this.methodNumCounter = methodNumCounter;
    }

    public void setFailCounter(JavaCGCounter failCounter) {
        this.failCounter = failCounter;
    }

    public void setFieldRelationshipCounter(JavaCGCounter fieldRelationshipCounter) {
        this.fieldRelationshipCounter = fieldRelationshipCounter;
    }

    public void setLastJarNum(int lastJarNum) {
        this.lastJarNum = lastJarNum;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
