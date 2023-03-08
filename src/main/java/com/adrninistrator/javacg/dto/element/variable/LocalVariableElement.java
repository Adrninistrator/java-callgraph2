package com.adrninistrator.javacg.dto.element.variable;

/**
 * @author adrninistrator
 * @date 2022/10/5
 * @description: 本地变量
 */
public class LocalVariableElement extends VariableElement {

    // 本地变量索引
    private final int index;

    public LocalVariableElement(String type, Object value, int index) {
        super(type);
        this.value = value;
        this.index = index;
    }

    public int getIndex() {
        return index;
    }

    // 复制一个值为null，其他字段相同的本地变量
    public LocalVariableElement copyWithNullValue() {
        return new LocalVariableElement(getType(), null, index);
    }

    // 判断是否为this
    public boolean isThis() {
        return index == 0;
    }
}
