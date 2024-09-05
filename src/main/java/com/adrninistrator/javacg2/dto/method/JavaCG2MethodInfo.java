package com.adrninistrator.javacg2.dto.method;

import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2021/8/22
 * @description: 方法信息
 */
public class JavaCG2MethodInfo {
    // 类名
    private final String className;

    // 方法名
    private final String methodName;

    // 方法参数类型
    private final Type[] methodArgumentTypes;

    // 方法返回类型
    private final Type methodReturnType;

    public JavaCG2MethodInfo(String className, String methodName, Type[] methodArgumentTypes, Type methodReturnType) {
        this.className = className;
        this.methodName = methodName;
        this.methodArgumentTypes = methodArgumentTypes;
        this.methodReturnType = methodReturnType;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }

    public Type[] getMethodArgumentTypes() {
        return methodArgumentTypes;
    }

    public Type getMethodReturnType() {
        return methodReturnType;
    }
}
