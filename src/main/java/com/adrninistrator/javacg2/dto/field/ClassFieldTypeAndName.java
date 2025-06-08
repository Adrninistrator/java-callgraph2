package com.adrninistrator.javacg2.dto.field;

import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2024/2/2
 * @description: 类中的字段类型及名称，包括静态字段及非静态字段
 */
public class ClassFieldTypeAndName extends FieldTypeAndName {

    // 类名
    private final String className;

    public ClassFieldTypeAndName(String fieldType, String fieldName, String className) {
        super(fieldType, fieldName);
        this.className = className;
    }

    /**
     * 获取类名+字段名
     *
     * @return 类名:字段名
     */
    public String getClassAndFieldName() {
        return JavaCG2ClassMethodUtil.formatClassAndField(className, fieldName);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        ClassFieldTypeAndName that = (ClassFieldTypeAndName) o;
        return fieldName.equals(that.fieldName) && className.equals(that.className);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fieldName, className);
    }

    public String getClassName() {
        return className;
    }
}
