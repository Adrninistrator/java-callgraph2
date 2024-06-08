package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.BootstrapMethod;
import org.apache.bcel.classfile.BootstrapMethods;
import org.apache.bcel.classfile.ClassFormatException;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantCP;
import org.apache.bcel.classfile.ConstantFieldref;
import org.apache.bcel.classfile.ConstantMethodHandle;
import org.apache.bcel.classfile.ConstantNameAndType;
import org.apache.bcel.classfile.ConstantPool;
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
public class JavaCGBootstrapMethodUtil {
    private static final Logger logger = LoggerFactory.getLogger(JavaCGBootstrapMethodUtil.class);

    /**
     * 获得JavaClass中指定下标的BootstrapMethod
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
    public static JavaCGMethodInfo getBootstrapMethodInfo(BootstrapMethod bootstrapMethod, JavaClass javaClass) {
        for (int argIndex : bootstrapMethod.getBootstrapArguments()) {
            Constant constantArg = javaClass.getConstantPool().getConstant(argIndex);
            if (!(constantArg instanceof ConstantMethodHandle)) {
                continue;
            }

            JavaCGMethodInfo javaCGMethodInfo = getMethodFromConstantMethodHandle((ConstantMethodHandle) constantArg, javaClass);
            if (javaCGMethodInfo != null) {
                return javaCGMethodInfo;
            }
        }

        return null;
    }

    /**
     * 根据ConstantMethodHandle获得Method对象
     *
     * @param constantMethodHandle
     * @param javaClass
     * @return
     */
    public static JavaCGMethodInfo getMethodFromConstantMethodHandle(ConstantMethodHandle constantMethodHandle, JavaClass javaClass) {
        ConstantPool constantPool = javaClass.getConstantPool();

        Constant constantCP = constantPool.getConstant(constantMethodHandle.getReferenceIndex());
        if (!(constantCP instanceof ConstantCP)) {
            logger.error("不满足instanceof ConstantCP {}", constantCP.getClass().getName());
            return null;
        }

        if (constantCP instanceof ConstantFieldref) {
            return null;
        }
        ConstantCP constantClassAndMethod = (ConstantCP) constantCP;
        String className = constantPool.getConstantString(constantClassAndMethod.getClassIndex(), Const.CONSTANT_Class);
        className = Utility.compactClassName(className, false);

        Constant constantNAT = constantPool.getConstant(constantClassAndMethod.getNameAndTypeIndex());
        if (!(constantNAT instanceof ConstantNameAndType)) {
            logger.error("不满足instanceof ConstantNameAndType {}", constantNAT.getClass().getName());
            return null;
        }
        ConstantNameAndType constantNameAndType = (ConstantNameAndType) constantNAT;
        String methodName = constantPool.constantToString(constantNameAndType.getNameIndex(), Const.CONSTANT_Utf8);
        String methodArgsReturn = constantPool.constantToString(constantNameAndType.getSignatureIndex(), Const.CONSTANT_Utf8);
        if (methodName != null && methodArgsReturn != null) {
            try {
                Type[] methodArgTypes = Type.getArgumentTypes(methodArgsReturn);
                Type returnType = Type.getReturnType(methodArgsReturn);
                return new JavaCGMethodInfo(className, methodName, methodArgTypes, returnType);
            } catch (ClassFormatException e) {
                logger.error("方法参数与返回类型不符合预期，不处理 {} {}", methodArgsReturn, constantClassAndMethod.getClass().getName());
                return null;
            }
        }

        logger.error("获取方法信息失败 {} {} {} {}", javaClass.getClassName(), className, methodName, methodArgsReturn);
        return null;
    }

    private JavaCGBootstrapMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
