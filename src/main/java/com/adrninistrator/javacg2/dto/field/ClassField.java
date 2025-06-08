package com.adrninistrator.javacg2.dto.field;

/**
 * @author adrninistrator
 * @date 2025/2/21
 * @description: 类中的字段
 */
public class ClassField {

    // 类名
    private String className;

    // 字段名称
    private String fieldName;

    // 字段类型
    private String fieldType;

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

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }
}
