package com.adrninistrator.javacg.dto.field;

import org.apache.commons.lang3.StringUtils;

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

    /**
     * 比较与另一个准备添加的对象值是否相同
     *
     * @param other
     * @return false: 不相同 true: 相同
     */
    public boolean compare(FieldTypeAndName other) {
        if (other == null) {
            return false;
        }

        return StringUtils.equals(fieldType, other.fieldType) && StringUtils.equals(fieldName, other.fieldName);
    }

    //
    public String getFieldType() {
        return fieldType;
    }

    public String getFieldName() {
        return fieldName;
    }
}
