package com.adrninistrator.javacg.enums;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量类型枚举
 */
public enum ConstantTypeEnum {
    // 以下都使用实际的类型，用于进行比较
    CONSTTE_NULL("null"),
    CONSTTE_INT("int"),
    CONSTTE_LONG("long"),
    CONSTTE_FLOAT("float"),
    CONSTTE_DOUBLE("double"),
    CONSTTE_BYTE("byte"),
    CONSTTE_CHAR("char"),
    CONSTTE_SHORT("short"),
    CONSTTE_STRING(JavaCGCommonNameConstants.CLASS_NAME_STRING),
    CONSTTE_BOOLEAN("boolean"),
    CONSTTE_ILLEGAL("ILLEGAL"),
    ;

    private final String type;

    ConstantTypeEnum(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

    public static ConstantTypeEnum getFromType(String type) {
        for (ConstantTypeEnum constantTypeEnum : ConstantTypeEnum.values()) {
            if (constantTypeEnum.getType().equals(type)) {
                return constantTypeEnum;
            }
        }
        return ConstantTypeEnum.CONSTTE_ILLEGAL;
    }

    @Override
    public String toString() {
        return type;
    }
}
