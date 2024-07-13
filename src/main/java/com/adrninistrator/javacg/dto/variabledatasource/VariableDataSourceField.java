package com.adrninistrator.javacg.dto.variabledatasource;

/**
 * @author adrninistrator
 * @date 2024/6/10
 * @description: 变量的数据来源，使用字段
 */
public class VariableDataSourceField extends AbstractVariableDataSource {

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
