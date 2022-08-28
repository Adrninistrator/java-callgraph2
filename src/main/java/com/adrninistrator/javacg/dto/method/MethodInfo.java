package com.adrninistrator.javacg.dto.method;

import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2021/8/22
 * @description:
 */
public class MethodInfo {

    private String className;

    private String methodName;

    private Type[] methodArgumentTypes;

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
}
