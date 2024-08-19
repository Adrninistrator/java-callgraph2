package com.adrninistrator.javacg.dto.element.variable;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 字段
 */
public class FieldElement extends LocalVariableElement {

    // 类名或this
    protected final String className;

    public FieldElement(String type, boolean arrayElement, Object value, String name, String className) {
        super(type, arrayElement, value, JavaCGConstants.LOCAL_VARIABLE_INDEX_NOT_USED, name);
        this.className = className;
    }

    public FieldElement(String type, boolean arrayElement, Object value, int index, String name, String className) {
        super(type, arrayElement, value, index, name);
        this.className = className;
    }

    @Override
    public BaseElement copyElement() {
        return copyFieldElement();
    }

    public FieldElement copyFieldElement() {
        FieldElement fieldElementCopy = new FieldElement(getType(), arrayElement, value, getIndex(), getName(), className);
        fieldElementCopy.copyVariableDataSource(this);
        return fieldElementCopy;
    }

    public String getClassName() {
        return className;
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
