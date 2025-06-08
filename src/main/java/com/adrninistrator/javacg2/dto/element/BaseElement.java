package com.adrninistrator.javacg2.dto.element;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
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

    // 当前变量的数组维度，0代表不属于数组
    private int arrayDimensions = 0;

    /*
        数组对应的值
        key
            数组序号
        value
            数组的值
     */
    private Map<Integer, BaseElement> arrayValueMap;

    // 变量或常量的常量值
    protected Object value;

    protected BaseElement() {
    }

    /**
     * 被操作元素的基类构造函数，由于当前构造函数中会根据类型字符串判断是否属于数组形式，因此外层可以不判断类型字符串
     *
     * @param type               类型
     * @param addArrayDimensions 需要增加的数组维度值，正数代表增加数组维度，0代表不不改变数组维度，负数代表减少数组维度
     */
    protected BaseElement(String type, int addArrayDimensions) {
        if (JavaCG2ConstantTypeEnum.CONSTTE_NULL.getType().equals(type)) {
            // 若类型为null，则记录数组维度为0，记录对应类型
            arrayDimensions = 0;
            this.type = type;
            return;
        }

        arrayDimensions = JavaCG2ByteCodeUtil.getTypeArrayDimensions(type) + addArrayDimensions;
        if (arrayDimensions < 0) {
            throw new JavaCG2RuntimeException("数组维度非法 " + arrayDimensions);
        }
        if (arrayDimensions > 0) {
            arrayValueMap = new HashMap<>();
        }
        if (addArrayDimensions == 0) {
            this.type = type;
        } else if (addArrayDimensions > 0) {
            // 增加数组维度
            this.type = JavaCG2ByteCodeUtil.addArrayFlag(type, addArrayDimensions);
        } else {
            // 减少数组维度
            this.type = JavaCG2ByteCodeUtil.removeOneArrayFlag(type);
        }
    }

    public abstract BaseElement copyElement();

    public int getElementSize() {
        return JavaCG2ByteCodeUtil.getTypeSize(getType());
    }

    public void checkTypeString(String expectedType) {
        if (!logger.isDebugEnabled()) {
            return;
        }

        JavaCG2ByteCodeUtil.checkTypeString(getType(), expectedType);
    }

    /**
     * 判断是否为数组类型
     *
     * @return
     */
    public boolean checkArrayElement() {
        return arrayDimensions > 0;
    }

    /**
     * 记录数组指定序号的元素
     *
     * @param index
     * @param value
     */
    public void setElement(int index, BaseElement value) {
        if (!checkArrayElement()) {
            throw new JavaCG2RuntimeException("当前类不是数组类型");
        }
        arrayValueMap.put(index, value);
    }

    protected void setType(String type) {
        this.type = type;
    }

    protected void setArrayDimensions(int arrayDimensions) {
        this.arrayDimensions = arrayDimensions;
    }

    protected void setValue(Object value) {
        this.value = value;
    }

    //
    public String getType() {
        return type;
    }

    public int getArrayDimensions() {
        return arrayDimensions;
    }

    public Map<Integer, BaseElement> getArrayValueMap() {
        return arrayValueMap;
    }

    public void setArrayValueMap(Map<Integer, BaseElement> arrayValueMap) {
        this.arrayValueMap = arrayValueMap;
    }

    public Object getValue() {
        return value;
    }

    @Override
    public String toString() {
        return simpleClassName + " type: " + getType() + " value: " + value;
    }
}
