package com.adrninistrator.javacg.dto.element;

import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 被操作元素的基类
 */
public abstract class BaseElement {
    protected String simpleClassName = this.getClass().getSimpleName();

    /*
        变量或常量的类型
        type需要使用getType()方法，因为ConstElement是通过方法返回的type
     */
    private String type;

    // 变量或常量的常量值
    protected Object value;

    public BaseElement() {
    }

    public BaseElement(String type) {
        this.type = type;
    }

    public int getElementSize() {
        return JavaCGByteCodeUtil.getTypeSize(getType());
    }

    public void checkTypeString(String expectedType) {
        if (!JavaCGLogUtil.isDebugPrintFlag()) {
            return;
        }

        String actualType = getType();
        if (JavaCGByteCodeUtil.isNullType(actualType)) {
            return;
        }

        if (JavaCGByteCodeUtil.compareType(actualType, expectedType)) {
            return;
        }

        JavaCGLogUtil.debugPrint("eee 类型与预期的不一致" + this + " expectedType: " + expectedType);
        System.err.println("eee 类型与预期的不一致\n" + this + "\nexpectedType:" + expectedType);
    }

    @Override
    public String toString() {
        return simpleClassName + " type: " + getType() + " value: " + value + String.format(" (%x)", System.identityHashCode(this));
    }

    public String getType() {
        return type;
    }

    public Object getValue() {
        return value;
    }
}
