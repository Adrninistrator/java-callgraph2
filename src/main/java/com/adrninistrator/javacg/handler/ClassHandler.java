package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGAnnotationUtil;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import copy.javassist.bytecode.BadBytecode;
import copy.javassist.bytecode.SignatureAttribute;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.ClassFormatException;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.commons.lang3.ArrayUtils;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
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

    private final JavaClass javaClass;

    private final ConstantPoolGen cpg;

    private final Set<String> handledMethodNameAndArgs;

    private UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;

    private ExtensionsManager extensionsManager;

    private JavaCGCounter callIdCounter;

    private JavaCGCounter methodNumCounter;

    private Writer classNameWriter;
    private Writer methodCallWriter;
    private Writer lambdaMethodInfoWriter;
    private Writer classAnnotationWriter;
    private Writer methodAnnotationWriter;
    private Writer methodLineNumberWriter;
    private Writer methodCallInfoWriter;
    private Writer methodInfoWriter;
    private Writer methodArgGenericsTypeWriter;
    private Writer methodReturnGenericsTypeWriter;

    private AnnotationAttributesFormatterInterface annotationAttributesFormatter;

    private final JavaCGConfInfo javaCGConfInfo;

    private int lastJarNum;

    private ClassAndJarNum classAndJarNum;

    // 非静态字段字段所有可能的类型
    private FieldPossibleTypes nonStaticFieldPossibleTypes;

    public ClassHandler(JavaClass javaClass, JavaCGConfInfo javaCGConfInfo) {
        this.javaClass = javaClass;
        this.javaCGConfInfo = javaCGConfInfo;
        cpg = new ConstantPoolGen(javaClass.getConstantPool());
        handledMethodNameAndArgs = new HashSet<>();

        if (javaCGConfInfo.isFirstParseInitMethodType()) {
            nonStaticFieldPossibleTypes = new FieldPossibleTypes();
        }
    }

    // 记录类之间引用关系
    private void recordReferencedClass() throws IOException {
        String className = javaClass.getClassName();
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
            if (!JavaCGUtil.isObjectClass(referencedClass)) {
                // 只处理非Object类的引用，去除类名中的数组形式
                referencedClass = JavaCGByteCodeUtil.removeArrayInClassName(referencedClass);
                referencedClassSet.add(referencedClass);
            }
        }

        // 首先写入当前类的类名
        if (!JavaCGUtil.checkSkipClass(className, javaCGConfInfo.getNeedHandlePackageSet())) {
            JavaCGFileUtil.write2FileWithTab(classNameWriter, JavaCGConstants.FLAG_HASHTAG);
            JavaCGFileUtil.write2FileWithTab(classNameWriter, className);
        }

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
            JavaCGAnnotationUtil.writeAnnotationInfo(javaClass.getClassName(),
                    javaClass.getAnnotationEntries(),
                    annotationAttributesFormatter,
                    classAnnotationWriter);
        }

        if (javaCGConfInfo.isFirstParseInitMethodType()) {
            // 解析构造函数
            for (Method method : javaClass.getMethods()) {
                if ("<init>".equals(method.getName()) && !parseInitMethod(method)) {
                    return false;
                }
            }
        }

        // 处理方法
        for (Method method : javaClass.getMethods()) {
            if (!handleMethod(method)) {
                return false;
            }
        }

        return true;
    }

    // 解析构造函数
    private boolean parseInitMethod(Method method) {
        MethodGen mg = new MethodGen(method, javaClass.getClassName(), cpg);
        MethodHandler4TypeAndValue methodHandler4TypeAndValue = new MethodHandler4TypeAndValue(mg, javaClass, javaCGConfInfo);
        methodHandler4TypeAndValue.setRecordFieldPossibleTypeFlag(true);
        methodHandler4TypeAndValue.setUseFieldPossibleTypeFlag(false);
        methodHandler4TypeAndValue.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);

        return methodHandler4TypeAndValue.handleMethod();
    }

    /**
     * 处理方法
     *
     * @param method
     * @return false: 处理失败 true: 处理成功
     */
    private boolean handleMethod(Method method) throws IOException {
        methodNumCounter.addAndGet();
        // 是否出现方法名+参数类型均相同的方法标记
        boolean existsSameMethodNameAndArgs = false;

        String className = javaClass.getClassName();
        // 生成格式化后的方法参数
        String callerMethodArgs = JavaCGMethodUtil.getArgListStr(method.getArgumentTypes());
        // 生成格式化后的完整方法
        String fullMethod = JavaCGMethodUtil.formatFullMethod(className, method.getName(), callerMethodArgs);
        // 返回类型
        String returnType = method.getReturnType().toString();
        String methodNameAndArgs = method.getName() + callerMethodArgs;
        if (!handledMethodNameAndArgs.contains(methodNameAndArgs)) {
            // 记录方法的信息
            JavaCGFileUtil.write2FileWithTab(methodInfoWriter, fullMethod, String.valueOf(method.getAccessFlags()), returnType);
        } else {
            existsSameMethodNameAndArgs = true;
            if (!JavaCGByteCodeUtil.isBridgeFlag(method.getAccessFlags())) {
                System.err.println("出现方法名+参数类型均相同的方法，但方法没有ACC_BRIDGE标志，与预期不符 " + className + " " + methodNameAndArgs);
            }
        }

        String methodGenericSignature = method.getGenericSignature();
        if (methodGenericSignature != null) {
            try {
                SignatureAttribute.MethodSignature methodSignature = SignatureAttribute.toMethodSignature(methodGenericSignature);
                // 记录方法参数中泛型类型
                recordMethodReturnGenericsType(fullMethod, methodSignature);

                // 记录方法参数中泛型类型
                recordMethodArgsGenericsType(fullMethod, methodSignature);
            } catch (BadBytecode e) {
                System.err.println("处理方法签名出现异常: " + fullMethod);
                e.printStackTrace();
            }
        }

        // 处理方法调用
        MethodGen mg = new MethodGen(method, className, cpg);
        MethodHandler4Invoke methodHandler4Invoke = new MethodHandler4Invoke(mg,
                javaClass,
                javaCGConfInfo,
                callerMethodArgs,
                fullMethod,
                useSpringBeanByAnnotationHandler,
                callIdCounter);
        methodHandler4Invoke.setRunnableImplClassMap(runnableImplClassMap);
        methodHandler4Invoke.setCallableImplClassMap(callableImplClassMap);
        methodHandler4Invoke.setTransactionCallbackImplClassMap(transactionCallbackImplClassMap);
        methodHandler4Invoke.setTransactionCallbackWithoutResultChildClassMap(transactionCallbackWithoutResultChildClassMap);
        methodHandler4Invoke.setThreadChildClassMap(threadChildClassMap);
        methodHandler4Invoke.setExtensionsManager(extensionsManager);
        methodHandler4Invoke.setMethodCallWriter(methodCallWriter);
        methodHandler4Invoke.setLambdaMethodInfoWriter(lambdaMethodInfoWriter);
        methodHandler4Invoke.setMethodAnnotationWriter(methodAnnotationWriter);
        methodHandler4Invoke.setMethodLineNumberWriter(methodLineNumberWriter);
        methodHandler4Invoke.setMethodCallInfoWriter(methodCallInfoWriter);
        methodHandler4Invoke.setParseMethodCallTypeValueFlag(javaCGConfInfo.isParseMethodCallTypeValue());
        methodHandler4Invoke.setLastJarNum(lastJarNum);
        methodHandler4Invoke.setExistsSameMethodNameAndArgs(existsSameMethodNameAndArgs);
        methodHandler4Invoke.setClassAndJarNum(classAndJarNum);

        if (javaCGConfInfo.isFirstParseInitMethodType()) {
            methodHandler4Invoke.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);
        }

        handledMethodNameAndArgs.add(methodNameAndArgs);

        return methodHandler4Invoke.handleMethod();
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
            parseMethodArgsGenericsType(classType, true, methodReturnGenericsTypeList);
            if (methodReturnGenericsTypeList.isEmpty()) {
                // 未获取到方法返回中泛型类型
                return;
            }

            // 记录返回类型
            JavaCGFileUtil.write2FileWithTab(methodReturnGenericsTypeWriter, fullMethod, JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_TYPE, String.valueOf(0), classType.getName());

            // 获取到方法返回中泛型类型，记录
            for (int i = 0; i < methodReturnGenericsTypeList.size(); i++) {
                JavaCGFileUtil.write2FileWithTab(methodReturnGenericsTypeWriter, fullMethod, JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_GENERICS_TYPE, String.valueOf(i),
                        methodReturnGenericsTypeList.get(i));
            }
        } catch (Exception e) {
            e.printStackTrace();
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
            parseMethodArgsGenericsType(classType, true, methodArgsGenericsTypeList);
            if (methodArgsGenericsTypeList.isEmpty()) {
                // 未获取到方法参数中泛型类型
                continue;
            }

            // 记录参数类型
            JavaCGFileUtil.write2FileWithTab(methodArgGenericsTypeWriter, fullMethod, String.valueOf(i), JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_TYPE, String.valueOf(0),
                    classType.getName());

            // 获取到方法参数中泛型类型，记录
            for (int j = 0; j < methodArgsGenericsTypeList.size(); j++) {
                JavaCGFileUtil.write2FileWithTab(methodArgGenericsTypeWriter, fullMethod, String.valueOf(i), JavaCGConstants.FILE_KEY_METHOD_ARGS_RETURN_GENERICS_TYPE,
                        String.valueOf(j), methodArgsGenericsTypeList.get(j));
            }
        }
    }

    /**
     * 解析方法参数中泛型类型
     *
     * @param type
     * @param outer                      是否为外层的数据
     * @param methodArgsGenericsTypeList
     */
    private void parseMethodArgsGenericsType(SignatureAttribute.Type type, boolean outer, List<String> methodArgsGenericsTypeList) {
        if (!(type instanceof SignatureAttribute.ClassType)) {
            return;
        }

        SignatureAttribute.ClassType classType = (SignatureAttribute.ClassType) type;
        // 获取参数类型
        SignatureAttribute.TypeArgument[] typeArguments = classType.getTypeArguments();
        if (ArrayUtils.isEmpty(typeArguments)) {
            if (outer) {
                // 外层的数据，没有泛型类型，不需要记录，返回
                return;
            }

            // 当前参数类型下不再有类型
            methodArgsGenericsTypeList.add(classType.getName());
            return;
        }

        if (!outer) {
            // 内层的数据，当前参数类型下还有类型，记录当前参数的类型，如List、Map
            methodArgsGenericsTypeList.add(classType.getName());
        }
        // 遍历参数类型
        for (SignatureAttribute.TypeArgument typeArgument : typeArguments) {
            // 递归处理，泛型类型需要记录
            parseMethodArgsGenericsType(typeArgument.getType(), false, methodArgsGenericsTypeList);
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

    public void setMethodLineNumberWriter(Writer methodLineNumberWriter) {
        this.methodLineNumberWriter = methodLineNumberWriter;
    }

    public void setMethodCallInfoWriter(Writer methodCallInfoWriter) {
        this.methodCallInfoWriter = methodCallInfoWriter;
    }

    public void setMethodInfoWriter(Writer methodInfoWriter) {
        this.methodInfoWriter = methodInfoWriter;
    }

    public void setMethodArgGenericsTypeWriter(Writer methodArgGenericsTypeWriter) {
        this.methodArgGenericsTypeWriter = methodArgGenericsTypeWriter;
    }

    public void setMethodReturnGenericsTypeWriter(Writer methodReturnGenericsTypeWriter) {
        this.methodReturnGenericsTypeWriter = methodReturnGenericsTypeWriter;
    }

    public void setMethodNumCounter(JavaCGCounter methodNumCounter) {
        this.methodNumCounter = methodNumCounter;
    }

    public void setLastJarNum(int lastJarNum) {
        this.lastJarNum = lastJarNum;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
