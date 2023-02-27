package com.adrninistrator.javacg.dto.element.variable;

import com.adrninistrator.javacg.common.JavaCGConstants;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 静态字段
 */
public class StaticFieldElement extends FieldElement {

    // 类名
    private final String className;

    public StaticFieldElement(String type, Object value, String variableName, String className) {
        super(type, value, variableName);

        this.className = className;
    }

    public StaticFieldElement(String type, Object value, String variableName, String className, int index) {
        super(type, value, variableName, index);

        this.className = className;
    }

    public String getClassName() {
        return className;
    }

    /**
     * 获取类名+字段名
     *
     * @return 类名:字段名
     */
    public String getClassAndFieldName() {
        return className + JavaCGConstants.FLAG_COLON + fieldName;
    }

    @Override
    public String toString() {
        return "StaticFieldElement{" +
                "simpleClassName='" + simpleClassName + '\'' +
                ", type='" + type + '\'' +
                ", value=" + value +
                ", fieldName='" + fieldName + '\'' +
                ", className='" + className + '\'' +
                '}' + String.format(" (%x)", System.identityHashCode(this));
    }
}
