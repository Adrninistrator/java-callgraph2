package com.adrninistrator.javacg2.dto.method;

import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2021/8/22
 * @description: 方法信息
 */
public class JavaCG2MethodInfo {
    // 类名
    private String className;

    // 方法名
    private String methodName;

    // 方法参数类型
    private Type[] methodArgumentTypes;

    // 方法返回类型
    private Type methodReturnType;

    // 类的数组维，0代表非数组
    private int arrayDimensions;

    public JavaCG2MethodInfo(String className, String methodName, Type[] methodArgumentTypes, Type methodReturnType, int arrayDimensions) {
        this.className = className;
        this.methodName = methodName;
        this.methodArgumentTypes = methodArgumentTypes;
        this.methodReturnType = methodReturnType;
        this.arrayDimensions = arrayDimensions;
    }

    public JavaCG2MethodInfo(String className, String methodName, Type[] methodArgumentTypes, Type methodReturnType) {
        this.className = className;
        this.methodName = methodName;
        this.methodArgumentTypes = methodArgumentTypes;
        this.methodReturnType = methodReturnType;
        this.arrayDimensions = 0;
    }

    public JavaCG2MethodInfo copy() {
        return new JavaCG2MethodInfo(this.className, this.methodName, this.methodArgumentTypes, this.methodReturnType);
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public Type[] getMethodArgumentTypes() {
        return methodArgumentTypes;
    }

    public void setMethodArgumentTypes(Type[] methodArgumentTypes) {
        this.methodArgumentTypes = methodArgumentTypes;
    }

    public Type getMethodReturnType() {
        return methodReturnType;
    }

    public void setMethodReturnType(Type methodReturnType) {
        this.methodReturnType = methodReturnType;
    }

    public int getArrayDimensions() {
        return arrayDimensions;
    }

    public void setArrayDimensions(int arrayDimensions) {
        this.arrayDimensions = arrayDimensions;
    }

    @Override
    public String toString() {
        return className + " " + methodName;
    }
}
