package com.adrninistrator.javacg2.dto.field;

/**
 * @author adrninistrator
 * @date 2025/2/19
 * @description: 对类中字段的方法调用
 */
public class ClassFieldMethodCall extends ClassField {

    // 被调用的方法名称
    private String methodName;

    // 被调用的方法参数类型
    private String[] argTypes;

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
}
