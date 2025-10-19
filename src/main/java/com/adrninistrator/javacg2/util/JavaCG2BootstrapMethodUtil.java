package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.dto.method.JavaCG2BootstrapMethod;
import com.adrninistrator.javacg2.dto.method.JavaCG2MethodInfo;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.BootstrapMethod;
import org.apache.bcel.classfile.BootstrapMethods;
import org.apache.bcel.classfile.ClassFormatException;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantCP;
import org.apache.bcel.classfile.ConstantFieldref;
import org.apache.bcel.classfile.ConstantMethodHandle;
import org.apache.bcel.classfile.ConstantMethodType;
import org.apache.bcel.classfile.ConstantNameAndType;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.ConstantUtf8;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2022/10/7
 * @description:
 */
public class JavaCG2BootstrapMethodUtil {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2BootstrapMethodUtil.class);

    /**
     * 获得JavaClass中指定序号的BootstrapMethod
     *
     * @param javaClass
     * @param index
     * @return
     */
    public static BootstrapMethod getBootstrapMethod(JavaClass javaClass, int index) {
        for (Attribute attribute : javaClass.getAttributes()) {
            if (attribute instanceof BootstrapMethods) {
                BootstrapMethods bootstrapMethods = (BootstrapMethods) attribute;
                BootstrapMethod[] bootstrapMethodArray = bootstrapMethods.getBootstrapMethods();
                if (bootstrapMethodArray != null && bootstrapMethodArray.length > index) {
                    return bootstrapMethodArray[index];
                }
            }
        }
        return null;
    }

    /**
     * 获得BootstrapMethod的方法信息
     *
     * @param bootstrapMethod
     * @param javaClass
     * @return
     */
    public static JavaCG2BootstrapMethod getBootstrapMethodInfo(BootstrapMethod bootstrapMethod, JavaClass javaClass) {
        ConstantPool constantPool = javaClass.getConstantPool();
        Constant constantBootstrapMethodRef = constantPool.getConstant(bootstrapMethod.getBootstrapMethodRef());
        if (!(constantBootstrapMethodRef instanceof ConstantMethodHandle)) {
            logger.warn("获取bootstrapMethod方法引用类型与预期不一致 {}", constantBootstrapMethodRef.getClass().getName());
            return null;
        }

        JavaCG2BootstrapMethod javaCG2BootstrapMethod = new JavaCG2BootstrapMethod();

        ConstantMethodHandle constantMethodHandle = (ConstantMethodHandle) constantBootstrapMethodRef;
        // 根据ConstantMethodHandle获得Method对象，bootstrap方法
        JavaCG2MethodInfo bootstrapMethodInfo = getMethodFromConstantMethodHandle(constantMethodHandle, javaClass);
        javaCG2BootstrapMethod.setBootstrapMethodInfo(bootstrapMethodInfo);

        if (bootstrapMethodInfo != null && JavaCG2CommonNameConstants.CLASS_NAME_LAMBDA_META_FACTORY.equals(bootstrapMethodInfo.getClassName()) &&
                JavaCG2CommonNameConstants.METHOD_NAME_META_FACTORY.equals(bootstrapMethodInfo.getMethodName())) {
            for (int argIndex : bootstrapMethod.getBootstrapArguments()) {
                Constant constantArg = constantPool.getConstant(argIndex);
                if (constantArg instanceof ConstantMethodHandle) {
                    // 根据ConstantMethodHandle获得Method对象，lambda表达式方法
                    JavaCG2MethodInfo lambdaMethodInfo = getMethodFromConstantMethodHandle((ConstantMethodHandle) constantArg, javaClass);
                    if (lambdaMethodInfo != null) {
                        javaCG2BootstrapMethod.setLambdaMethodInfo(lambdaMethodInfo);
                    }
                } else if (constantArg instanceof ConstantMethodType) {
                    ConstantMethodType constantMethodType = (ConstantMethodType) constantArg;
                    Constant constantArgUtf8 = constantPool.getConstant(constantMethodType.getDescriptorIndex());
                    if (!(constantArgUtf8 instanceof ConstantUtf8)) {
                        logger.warn("constantUtf8 类型不符合预期 {}", constantArgUtf8.getClass().getName());
                    } else {
                        // Lambda表达式方法参数类型使用第一次出现的ConstantMethodType对应的
                        ConstantUtf8 constantUtf8 = (ConstantUtf8) constantArgUtf8;
                        String utf8Value = constantUtf8.getBytes();
                        Type[] methodArgTypes = Type.getArgumentTypes(utf8Value);
                        Type methodReturnType = Type.getReturnType(utf8Value);
                        if (javaCG2BootstrapMethod.getLambdaMethodActualArgumentTypes() == null) {
                            javaCG2BootstrapMethod.setLambdaMethodActualArgumentTypes(methodArgTypes);
                        }
                        if (javaCG2BootstrapMethod.getLambdaMethodActualReturnType() == null) {
                            javaCG2BootstrapMethod.setLambdaMethodActualReturnType(methodReturnType);
                        }
                    }
                }
            }
        }

        return javaCG2BootstrapMethod;
    }

    /**
     * 根据ConstantMethodHandle获得Method对象
     *
     * @param constantMethodHandle
     * @param javaClass
     * @return
     */
    public static JavaCG2MethodInfo getMethodFromConstantMethodHandle(ConstantMethodHandle constantMethodHandle, JavaClass javaClass) {
        ConstantPool constantPool = javaClass.getConstantPool();

        Constant constantCP = constantPool.getConstant(constantMethodHandle.getReferenceIndex());
        if (!(constantCP instanceof ConstantCP)) {
            logger.warn("不满足instanceof ConstantCP {}", constantCP.getClass().getName());
            return null;
        }

        if (constantCP instanceof ConstantFieldref) {
            logger.warn("constantCP 类型为 {}", constantCP.getClass().getName());
            return null;
        }
        ConstantCP constantClassAndMethod = (ConstantCP) constantCP;
        String className = constantPool.getConstantString(constantClassAndMethod.getClassIndex(), Const.CONSTANT_Class);
        className = Utility.compactClassName(className, false);

        Constant constantNAT = constantPool.getConstant(constantClassAndMethod.getNameAndTypeIndex());
        if (!(constantNAT instanceof ConstantNameAndType)) {
            logger.warn("不满足instanceof ConstantNameAndType {}", constantNAT.getClass().getName());
            return null;
        }
        ConstantNameAndType constantNameAndType = (ConstantNameAndType) constantNAT;
        String methodName = constantPool.constantToString(constantNameAndType.getNameIndex(), Const.CONSTANT_Utf8);
        String methodArgsReturn = constantPool.constantToString(constantNameAndType.getSignatureIndex(), Const.CONSTANT_Utf8);
        if (methodName != null && methodArgsReturn != null) {
            try {
                Type[] methodArgTypes = Type.getArgumentTypes(methodArgsReturn);
                Type returnType = Type.getReturnType(methodArgsReturn);
                return new JavaCG2MethodInfo(className, methodName, methodArgTypes, returnType);
            } catch (ClassFormatException e) {
                logger.warn("方法参数与返回类型不符合预期，不处理 {} {}", methodArgsReturn, constantClassAndMethod.getClass().getName());
                return null;
            }
        }

        logger.warn("获取方法信息失败 {} {} {} {}", javaClass.getClassName(), className, methodName, methodArgsReturn);
        return null;
    }

    private JavaCG2BootstrapMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
