package com.adrninistrator.javacg2.dto.field;

/**
 * @author adrninistrator
 * @date 2025/2/19
 * @description: 对类中静态字段的方法调用
 */
public class StaticFieldMethodCall {

    // 被调用的类名
    private String className;

    // 被调用的字段名称
    private String fieldName;

    // 被调用的方法名称
    private String methodName;

    // 被调用的方法参数类型
    private String[] argTypes;

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

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
