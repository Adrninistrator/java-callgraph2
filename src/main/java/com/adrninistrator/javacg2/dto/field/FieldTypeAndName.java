package com.adrninistrator.javacg2.dto.field;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2022/11/3
 * @description: 字段类型及名称
 */
public class FieldTypeAndName {

    // 字段类型
    protected final String fieldType;

    // 字段名称
    protected final String fieldName;

    public FieldTypeAndName(String fieldType, String fieldName) {
        this.fieldType = fieldType;
        this.fieldName = fieldName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FieldTypeAndName that = (FieldTypeAndName) o;
        return fieldName.equals(that.fieldName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fieldName);
    }

    @Override
    public String toString() {
        return "fieldType=" + fieldType +
                ", fieldName=" + fieldName;
    }

    //
    public String getFieldType() {
        return fieldType;
    }

    public String getFieldName() {
        return fieldName;
    }
}
