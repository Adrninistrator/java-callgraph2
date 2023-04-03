package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.ArrayUtils;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: 方法处理相关工具类
 */
public class JavaCGMethodUtil {

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
        return formatFullMethod(className, methodName, getArgListStr(arguments));
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param arguments  方法参数，包含起始的()，参数类名之间需要使用半角逗号,分隔，不能包含空格，参数类名也需要为完整类名
     * @return
     */
    public static String formatFullMethod(String className, String methodName, String arguments) {
        return className + JavaCGConstants.FLAG_COLON + methodName + arguments;
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatFullMethod(String className, String methodName, Class<?>... argTypes) {
        return formatFullMethodWithArgs(className, formatMethodWithArgs(methodName, argTypes));
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatFullMethodStr(String className, String methodName, String... argTypes) {
        return formatFullMethodWithArgs(className, formatMethodWithArgsStr(methodName, argTypes));
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
     * 生成格式化后的完整方法
     *
     * @param className         完整类名
     * @param methodNameAndArgs 方法名+方法参数
     * @return
     */
    public static String formatFullMethodWithArgs(String className, String methodNameAndArgs) {
        return className + JavaCGConstants.FLAG_COLON + methodNameAndArgs;
    }

    /**
     * 生成格式化后的方法名+方法参数
     *
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatMethodWithArgs(String methodName, Class<?>... argTypes) {
        StringBuilder sb = new StringBuilder(methodName).append(JavaCGConstants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCGConstants.FLAG_COMMA);
            }
            sb.append(argTypes[i].getName());
        }
        sb.append(JavaCGConstants.FLAG_RIGHT_BRACKET);
        return sb.toString();
    }

    /**
     * 生成格式化后的方法名+方法参数
     *
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatMethodWithArgsStr(String methodName, String... argTypes) {
        if (ArrayUtils.isEmpty(argTypes)) {
            return methodName + JavaCGConstants.EMPTY_METHOD_ARGS;
        }

        StringBuilder sb = new StringBuilder(methodName).append(JavaCGConstants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCGConstants.FLAG_COMMA);
            }
            sb.append(argTypes[i]);
        }
        sb.append(JavaCGConstants.FLAG_RIGHT_BRACKET);
        return sb.toString();
    }

    private JavaCGMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
