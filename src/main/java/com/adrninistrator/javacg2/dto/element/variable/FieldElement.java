package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 非静态字段
 */
public class FieldElement extends LocalVariableElement {

    // 类名或this
    protected final String className;

    /*
        rawType，代表非静态字段在定义时的原始类型
        父类中的type，对于非静态字段，代表当前实例化时使用的类型
     */
    protected final String rawType;

    public FieldElement(String type, int addArrayDimensions, Object value, String name, String className) {
        this(type, addArrayDimensions, value, JavaCG2Constants.LOCAL_VARIABLE_INDEX_NOT_USED, name, className, type);
    }

    public FieldElement(String type, int addArrayDimensions, Object value, int index, String name, String className) {
        this(type, addArrayDimensions, value, index, name, className, type);
    }

    public FieldElement(String type, int addArrayDimensions, Object value, int index, String name, String className, String rawType) {
        super(type, addArrayDimensions, value, index, name);
        this.className = className;
        this.rawType = rawType;
    }

    @Override
    public BaseElement copyElement() {
        FieldElement fieldElementCopy = new FieldElement(getType(), 0, value, getIndex(), getName(), className, rawType);
        fieldElementCopy.copyVariableDataSource(this);
        fieldElementCopy.setArrayValueMap(getArrayValueMap());
        return fieldElementCopy;
    }

    public String getClassName() {
        return className;
    }

    public String getRawType() {
        return rawType;
    }

    @Override
    public String toString() {
        return "FieldElement{" +
                "type='" + getType() + '\'' +
                ", value=" + value +
                ", name='" + getName() + '\'' +
                ", className='" + className + '\'';
    }
}
