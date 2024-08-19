package com.adrninistrator.javacg.dto.element.variable;

import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 静态字段
 */
public class StaticFieldElement extends FieldElement {

    public StaticFieldElement(String type, boolean arrayElement, Object value, String variableName, String className) {
        super(type, arrayElement, value, variableName, className);
    }

    public StaticFieldElement(String type, boolean arrayElement, Object value, int index, String variableName, String className) {
        super(type, arrayElement, value, index, variableName, className);
    }

    @Override
    public BaseElement copyElement() {
        StaticFieldElement staticFieldElementCopy = new StaticFieldElement(getType(), arrayElement, value, getIndex(), getName(), className);
        staticFieldElementCopy.copyVariableDataSource(this);
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
