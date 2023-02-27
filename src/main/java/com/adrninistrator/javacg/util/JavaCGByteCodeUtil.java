package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.TypeConstants;
import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import com.adrninistrator.javacg.dto.method.MethodAndArgs;
import com.adrninistrator.javacg.enums.ConstantTypeEnum;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumberTable;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.Signature;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.Type;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/9/21
 * @description:
 */
public class JavaCGByteCodeUtil {
    /**
     * 生成格式化后的完整方法
     *
     * @param javaCGMethodInfo 方法信息
     * @return
     */
    public static String formatFullMethod(JavaCGMethodInfo javaCGMethodInfo) {
        return formatFullMethod(javaCGMethodInfo.getClassName(), javaCGMethodInfo.getMethodName(), javaCGMethodInfo.getMethodArgumentTypes());
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param arguments  方法参数，包含起始的()，参数类名之间需要使用半角逗号,分隔，不能包含空格，参数类名也需要为完整类名
     * @return
     */
    public static String formatFullMethod(String className, String methodName, Type[] arguments) {
        return JavaCGUtil.formatFullMethod(className, methodName, getArgListStr(arguments));
    }

    /**
     * 生成参数的字符串形式
     *
     * @param arguments
     * @return
     */
    public static String getArgListStr(Type[] arguments) {
        StringBuilder sb = new StringBuilder(JavaCGConstants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < arguments.length; i++) {
            if (i != 0) {
                sb.append(JavaCGConstants.FLAG_COMMA);
            }
            sb.append(arguments[i].toString());
        }
        sb.append(JavaCGConstants.FLAG_RIGHT_BRACKET);
        return sb.toString();
    }

    /**
     * 获取可能涉及继承的相关方法，包含参数
     *
     * @param methods
     * @return
     */
    public static List<MethodAndArgs> genImplClassMethodWithArgs(Method[] methods) {
        List<MethodAndArgs> methodInfoList = new ArrayList<>(methods.length);
        for (Method method : methods) {
            String methodName = method.getName();
            // 忽略"<init>"和"<clinit>"方法
            if (!methodName.startsWith("<") && method.isPublic() && !method.isAbstract() && !method.isStatic()) {
                methodInfoList.add(new MethodAndArgs(methodName, getArgListStr(method.getArgumentTypes())));
            }
        }
        return methodInfoList;
    }

    /**
     * 获取接口相关的方法，包含参数
     *
     * @param methods
     * @return
     */
    public static List<MethodAndArgs> genInterfaceMethodWithArgs(Method[] methods) {
        List<MethodAndArgs> methodInfoList = new ArrayList<>(methods.length);
        for (Method method : methods) {
            methodInfoList.add(new MethodAndArgs(method.getName(), getArgListStr(method.getArgumentTypes())));
        }
        return methodInfoList;
    }

    /**
     * 获取Lambda表达式原始方法
     *
     * @param lambdaMethod
     * @return
     */
    public static String getLambdaOrigMethod(String lambdaMethod) {
        int indexLastLambda = lambdaMethod.lastIndexOf(JavaCGConstants.FLAG_LAMBDA);
        String tmpString = lambdaMethod.substring(indexLastLambda + JavaCGConstants.FLAG_LAMBDA.length());
        int indexDollar = tmpString.indexOf('$');
        return tmpString.substring(0, indexDollar);
    }

    /**
     * 获取方法的起始代码行号
     *
     * @param method
     * @return
     */
    public static int getFuncStartSourceLine(Method method) {
        LineNumberTable lineNumberTable = method.getLineNumberTable();
        if (lineNumberTable == null || lineNumberTable.getLineNumberTable() == null) {
            return JavaCGConstants.DEFAULT_LINE_NUMBER;
        }

        return lineNumberTable.getLineNumberTable()[0].getLineNumber();
    }

    /**
     * 获取构造函数的起始代码行号
     *
     * @param javaClass
     * @return
     */
    private static int getInitFuncStartSourceLine(JavaClass javaClass) {
        Method[] methods = javaClass.getMethods();
        if (methods == null) {
            return JavaCGConstants.DEFAULT_LINE_NUMBER;
        }

        for (Method method : methods) {
            if (JavaCGConstants.METHOD_NAME_INIT.equals(method.getName())) {
                return getFuncStartSourceLine(method);
            }
        }

        return JavaCGConstants.DEFAULT_LINE_NUMBER;
    }

    /**
     * 去除类名中的数组形式
     *
     * @param className
     * @return
     */
    public static String removeArrayInClassName(String className) {
        if (!className.startsWith("[")) {
            return className;
        }

        // 处理数组格式
        String tmpClassName = Utility.typeSignatureToString(className, false);
        return removeArrayFlag(tmpClassName);
    }

    /**
     * 获取对象对应的数组类型
     *
     * @param className
     * @return
     */
    public static String addArrayFlag(String className) {
        return className + JavaCGConstants.FLAG_ARRAY;
    }

    /**
     * 去年数组形式最后的[]
     *
     * @param arrayType
     * @return
     */
    public static String removeArrayFlag(String arrayType) {
        if (arrayType == null) {
            return null;
        }

        if (!arrayType.endsWith(JavaCGConstants.FLAG_ARRAY) && !JavaCGByteCodeUtil.isNullType(arrayType)) {
            System.err.println("类名不是数组形式 " + arrayType);
            return arrayType;
        }

        return arrayType.substring(0, arrayType.length() - JavaCGConstants.FLAG_ARRAY.length());
    }

    /**
     * 判断是否为数组类型
     *
     * @param type
     * @return
     */
    public static boolean isArrayType(String type) {
        return type.endsWith(JavaCGConstants.FLAG_ARRAY);
    }

    /**
     * 获取指定类型的长度
     *
     * @param typeString
     * @return
     */
    public static int getTypeSize(String typeString) {
        if (ConstantTypeEnum.CONSTTE_DOUBLE.getType().equals(typeString) || ConstantTypeEnum.CONSTTE_LONG.getType().equals(typeString)) {
            return 2;
        }
        return 1;
    }

    /**
     * 判断是否为与int兼容的类型
     *
     * @param type
     * @return false: 不类型 true: 类型
     */
    public static boolean checkCompatibleWithInt(String type) {
        for (String compatibleType : TypeConstants.COMPATIBLE_INT_TYPES) {
            if (compatibleType.equals(type)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 判断类型是否为null
     *
     * @param type
     * @return false: 不为null true: 为null
     */
    public static boolean isNullType(String type) {
        return type == null || ConstantTypeEnum.CONSTTE_NULL.getType().equals(type);
    }

    /**
     * 比较两个类型是否兼容
     *
     * @param type1 不允许为null
     * @param type2
     * @return false: 不兼容 true: 兼容
     */
    public static boolean compareType(String type1, String type2) {
        if (type1.equals(type2)) {
            return true;
        }

        if (compareIntType(type1, type2)) {
            return true;
        }

        return compareByteBooleanType(type1, type2);
    }

    /**
     * 比较指定的两个类型是否为与int兼容的类型
     *
     * @param type1
     * @param type2
     * @return false: 与int不兼容 true: 与int兼容
     */
    public static boolean compareIntType(String type1, String type2) {
        if (ConstantTypeEnum.CONSTTE_INT.getType().equals(type1) && JavaCGByteCodeUtil.checkCompatibleWithInt(type2)) {
            return true;
        }

        return ConstantTypeEnum.CONSTTE_INT.getType().equals(type2) && JavaCGByteCodeUtil.checkCompatibleWithInt(type1);
    }

    /**
     * 比较指定的两个类型是否为byte、boolean（或一维数组）兼容的类型
     *
     * @param type1
     * @param type2
     * @return false: 不兼容 true: 兼容
     */
    public static boolean compareByteBooleanType(String type1, String type2) {
        if (ConstantTypeEnum.CONSTTE_BYTE.getType().equals(type1) && ConstantTypeEnum.CONSTTE_BOOLEAN.getType().equals(type2)) {
            return true;
        }

        if (ConstantTypeEnum.CONSTTE_BYTE.getType().equals(type2) && ConstantTypeEnum.CONSTTE_BOOLEAN.getType().equals(type1)) {
            return true;
        }

        if (TypeConstants.BYTE_ARRAY_TYPE.equals(type1) && TypeConstants.BOOLEAN_ARRAY_TYPE.equals(type2)) {
            return true;
        }

        return TypeConstants.BYTE_ARRAY_TYPE.equals(type2) && TypeConstants.BOOLEAN_ARRAY_TYPE.equals(type1);
    }

    /**
     * 判断是否为public
     *
     * @param accessFlags
     * @return
     */
    public static boolean isPublicFlag(int accessFlags) {
        return (accessFlags & Const.ACC_PUBLIC) != 0;
    }

    /**
     * 判断是否为protected
     *
     * @param accessFlags
     * @return
     */
    public static boolean isProtectedMethod(int accessFlags) {
        return (accessFlags & Const.ACC_PROTECTED) != 0;
    }

    /**
     * 判断是否为abstract
     *
     * @param accessFlags
     * @return
     */
    public static boolean isAbstractFlag(int accessFlags) {
        return (accessFlags & Const.ACC_ABSTRACT) != 0;
    }

    /**
     * 判断是否有ACC_BRIDGE标志
     *
     * @param accessFlags
     * @return
     */
    public static boolean isBridgeFlag(int accessFlags) {
        return (accessFlags & Const.ACC_BRIDGE) != 0;
    }

    /**
     * 设置public标志
     *
     * @param accessFlags
     * @param trueOrFalse
     * @return
     */
    public static int setPublicFlag(int accessFlags, boolean trueOrFalse) {
        return setFlag(accessFlags, Const.ACC_PUBLIC, trueOrFalse);
    }

    /**
     * 设置protected标志
     *
     * @param accessFlags
     * @param trueOrFalse
     * @return
     */
    public static int setProtectedFlag(int accessFlags, boolean trueOrFalse) {
        return setFlag(accessFlags, Const.ACC_PROTECTED, trueOrFalse);
    }

    /**
     * 设置public标志
     *
     * @param accessFlags
     * @param trueOrFalse
     * @return
     */
    public static int setAbstractFlag(int accessFlags, boolean trueOrFalse) {
        return setFlag(accessFlags, Const.ACC_ABSTRACT, trueOrFalse);
    }

    /**
     * 设置指定位的标记
     *
     * @param accessFlags
     * @param flag
     * @param trueOrFalse
     * @return
     */
    private static int setFlag(int accessFlags, int flag, boolean trueOrFalse) {
        if ((accessFlags & flag) != 0) { // Flag is set already
            if (!trueOrFalse) {
                accessFlags ^= flag;
            }
        } else if (trueOrFalse) {
            accessFlags |= flag;
        }
        return accessFlags;
    }

    /**
     * 获取类的Signature
     *
     * @param javaClass
     * @return
     */
    public static Signature getSignatureOfClass(JavaClass javaClass) {
        for (Attribute attribute : javaClass.getAttributes()) {
            if (attribute instanceof Signature) {
                return (Signature) attribute;
            }
        }

        return null;
    }

    private JavaCGByteCodeUtil() {
        throw new IllegalStateException("illegal");
    }
}
