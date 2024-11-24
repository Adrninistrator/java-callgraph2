package com.adrninistrator.javacg2.dto.type;

/**
 * @author adrninistrator
 * @date 2024/11/14
 * @description: 包含数组维度的类型
 */
public class JavaCG2Type {

    // 类型
    protected String type;

    // 属于数组类型时的数组维度，0代表不是数组
    protected int arrayDimensions;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getArrayDimensions() {
        return arrayDimensions;
    }

    public void setArrayDimensions(int arrayDimensions) {
        this.arrayDimensions = arrayDimensions;
    }
}
