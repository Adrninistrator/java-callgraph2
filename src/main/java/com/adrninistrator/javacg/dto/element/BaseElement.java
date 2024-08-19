package com.adrninistrator.javacg.dto.element;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 被操作元素的基类
 */
public abstract class BaseElement {
    private static final Logger logger = LoggerFactory.getLogger(BaseElement.class);

    protected String simpleClassName = this.getClass().getSimpleName();

    /*
        变量或常量的类型
        type需要使用getType()方法，因为ConstElement是通过方法返回的type
     */
    private String type;

    // 当前变量是否属于数组类型
    protected boolean arrayElement = false;

    /*
        数组对应的值
        key
            数组下标
        value
            数组的值
     */
    private Map<Integer, BaseElement> arrayValueMap;

    // 变量或常量的常量值
    protected Object value;

    protected BaseElement() {
    }

    protected BaseElement(String type, boolean arrayElement) {
        this.type = type;
        /*
            当外部参数指定当前属于数组类型，或类型字符串属于数组形式时，都认为是数组类型
            由于当前构造函数中会根据类型字符串判断是否属于数组形式，因此外层可以不判断类型字符串
         */
        this.arrayElement = arrayElement || JavaCGByteCodeUtil.isArrayType(type);
        if (this.arrayElement) {
            arrayValueMap = new HashMap<>();
        }
    }

    public abstract BaseElement copyElement();

    public int getElementSize() {
        return JavaCGByteCodeUtil.getTypeSize(getType());
    }

    public void checkTypeString(String expectedType) {
        if (!logger.isDebugEnabled()) {
            return;
        }

        String actualType = getType();
        if (JavaCGByteCodeUtil.isNullType(actualType)) {
            return;
        }

        if (JavaCGByteCodeUtil.compareType(actualType, expectedType)) {
            return;
        }

        logger.error("类型与预期的不一致 {} expectedType: {}", this, expectedType);
    }

    /**
     * 记录数组指定下标的元素
     *
     * @param index
     * @param value
     */
    public void setElement(int index, BaseElement value) {
        if (!arrayElement) {
            throw new JavaCGRuntimeException("当前类不是数组类型");
        }
        arrayValueMap.put(index, value);
    }

    public Map<Integer, BaseElement> getArrayValueMap() {
        if (!arrayElement) {
            throw new JavaCGRuntimeException("当前类不是数组类型");
        }
        return arrayValueMap;
    }

    public void setArrayValueMap(Map<Integer, BaseElement> arrayValueMap) {
        this.arrayValueMap = arrayValueMap;
    }

    public String getType() {
        return type;
    }

    public boolean isArrayElement() {
        return arrayElement;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public String toString() {
        return simpleClassName + " type: " + getType() + " value: " + value;
    }
}
