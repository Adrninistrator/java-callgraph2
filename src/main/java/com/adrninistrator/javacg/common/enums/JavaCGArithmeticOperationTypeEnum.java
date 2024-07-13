package com.adrninistrator.javacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/8/5
 * @description: 算术运算类型枚举
 */
public enum JavaCGArithmeticOperationTypeEnum {
    AOTE_ADD("+"),
    AOTE_SUB("-"),
    AOTE_MUL("*"),
    AOTE_DIV("/"),
    ;

    private final String type;

    JavaCGArithmeticOperationTypeEnum(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

}
