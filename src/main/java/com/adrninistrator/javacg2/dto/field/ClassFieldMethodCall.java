package com.adrninistrator.javacg2.dto.field;

import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2025/2/19
 * @description: 对类中字段的方法调用
 */
public class ClassFieldMethodCall extends ClassField {

    // 被调用的方法名
    private String methodName;

    // 被调用的方法参数类型
    private String[] argTypes;

    // 被调用的方法返回类型
    private String returnType;

    public String genFullMethod() {
        return JavaCG2ClassMethodUtil.formatFullMethodStr(getClassName(), methodName, argTypes);
    }

    //
    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String[] getArgTypes() {
        return argTypes;
    }

    public void setArgTypes(String[] argTypes) {
        this.argTypes = argTypes;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }
}
