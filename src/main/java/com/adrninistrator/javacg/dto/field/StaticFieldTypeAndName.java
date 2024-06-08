package com.adrninistrator.javacg.dto.field;

import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2024/2/2
 * @description: 类中的静态字段类型及名称
 */
public class StaticFieldTypeAndName extends FieldTypeAndName {

    // 类名
    private final String className;

    public StaticFieldTypeAndName(String fieldType, String fieldName, String className) {
        super(fieldType, fieldName);
        this.className = className;
    }

    /**
     * 获取类名+字段名
     *
     * @return 类名:字段名
     */
    public String getClassAndFieldName() {
        return JavaCGClassMethodUtil.genClassAndField(className, fieldName);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        StaticFieldTypeAndName that = (StaticFieldTypeAndName) o;
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
