package com.adrninistrator.javacg.dto.method;

import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2021/8/22
 * @description: 方法信息
 */
public class JavaCGMethodInfo {
    private final String className;

    private final String methodName;

    private final Type[] methodArgumentTypes;

    public JavaCGMethodInfo(String className, String methodName, Type[] methodArgumentTypes) {
        this.className = className;
        this.methodName = methodName;
        this.methodArgumentTypes = methodArgumentTypes;
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
}
