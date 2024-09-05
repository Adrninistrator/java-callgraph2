package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 本地变量
 */
public class LocalVariableElement extends VariableElement {

    // 本地变量索引
    private final int index;

    // 名称
    private final String name;

    public LocalVariableElement(String type, boolean arrayElement, Object value, int index, String name) {
        super(type, arrayElement);
        this.value = value;
        this.index = index;
        this.name = name;
    }

    @Override
    public BaseElement copyElement() {
        return copyLocalVariableElement();
    }

    public LocalVariableElement copyLocalVariableElement() {
        LocalVariableElement localVariableElementCopy = new LocalVariableElement(getType(), arrayElement, value, index, name);
        localVariableElementCopy.copyVariableDataSource(this);
        return localVariableElementCopy;
    }

    // 复制一个值为null，其他字段相同的本地变量
    public LocalVariableElement copyWithNullValue() {
        return new LocalVariableElement(getType(), arrayElement, null, index, name);
    }

    // 判断是否为this
    public boolean isThis() {
        return index == 0;
    }

    public int getIndex() {
        return index;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return "LocalVariableElement{" +
                "type='" + getType() + '\'' +
                ", value=" + value +
                ", index=" + index +
                ", name='" + name + '\'' +
                '}';
    }
}
