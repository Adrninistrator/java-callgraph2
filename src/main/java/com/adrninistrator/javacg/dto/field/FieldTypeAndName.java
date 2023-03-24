package com.adrninistrator.javacg.dto.field;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2022/11/3
 * @description: 字段类型及名称
 */
public class FieldTypeAndName {

    // 字段类型
    private final String fieldType;

    // 字段名称
    private final String fieldName;

    public FieldTypeAndName(String fieldType, String fieldName) {
        this.fieldType = fieldType;
        this.fieldName = fieldName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FieldTypeAndName that = (FieldTypeAndName) o;
        return Objects.equals(fieldType, that.fieldType) && Objects.equals(fieldName, that.fieldName);
    }

    //
    public String getFieldType() {
        return fieldType;
    }

    public String getFieldName() {
        return fieldName;
    }
}
