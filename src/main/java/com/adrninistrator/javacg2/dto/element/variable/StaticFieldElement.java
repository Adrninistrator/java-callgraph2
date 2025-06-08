package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 静态字段
 */
public class StaticFieldElement extends FieldElement {

    public StaticFieldElement(String type, int addArrayDimensions, Object value, String variableName, String className) {
        super(type, addArrayDimensions, value, variableName, className);
    }

    public StaticFieldElement(String type, int addArrayDimensions, Object value, int index, String variableName, String className) {
        super(type, addArrayDimensions, value, index, variableName, className, type);
    }

    @Override
    public BaseElement copyElement() {
        StaticFieldElement staticFieldElementCopy = new StaticFieldElement(getType(), 0, value, getIndex(), getName(), className);
        staticFieldElementCopy.copyVariableDataSource(this);
        staticFieldElementCopy.setArrayValueMap(getArrayValueMap());
        return staticFieldElementCopy;
    }

    @Override
    public String toString() {
        return "StaticFieldElement{" +
                "simpleClassName='" + simpleClassName + '\'' +
                ", type='" + getType() + '\'' +
                ", value=" + value +
                ", name='" + getName() + '\'' +
                ", className='" + className + '\'';
    }
}
