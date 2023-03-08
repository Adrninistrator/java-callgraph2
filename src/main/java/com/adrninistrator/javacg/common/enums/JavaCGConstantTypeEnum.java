package com.adrninistrator.javacg.common.enums;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量类型枚举
 */
public enum JavaCGConstantTypeEnum {
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

    JavaCGConstantTypeEnum(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

    public static JavaCGConstantTypeEnum getFromType(String type) {
        for (JavaCGConstantTypeEnum constantTypeEnum : JavaCGConstantTypeEnum.values()) {
            if (constantTypeEnum.getType().equals(type)) {
                return constantTypeEnum;
            }
        }
        return JavaCGConstantTypeEnum.CONSTTE_ILLEGAL;
    }

    @Override
    public String toString() {
        return type;
    }
}
