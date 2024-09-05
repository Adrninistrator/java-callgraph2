package com.adrninistrator.javacg2.common.enums;

import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量类型枚举
 */
public enum JavaCG2ConstantTypeEnum {
    // 以下都使用实际的类型，用于进行比较
    CONSTTE_NULL("null", false),
    CONSTTE_INT("int", true),
    CONSTTE_LONG("long", true),
    CONSTTE_FLOAT("float", true),
    CONSTTE_DOUBLE("double", true),
    CONSTTE_BYTE("byte", true),
    CONSTTE_CHAR("char", true),
    CONSTTE_SHORT("short", true),
    CONSTTE_STRING(JavaCG2CommonNameConstants.CLASS_NAME_STRING, false),
    CONSTTE_BOOLEAN("boolean", true),
    CONSTTE_ILLEGAL("ILLEGAL", false),
    ;

    private final String type;
    private final boolean primitive;

    JavaCG2ConstantTypeEnum(String type, boolean primitive) {
        this.type = type;
        this.primitive = primitive;
    }

    public String getType() {
        return type;
    }

    public boolean isPrimitive() {
        return primitive;
    }

    /**
     * 获取常量类型枚举
     *
     * @param type
     * @return
     */
    public static JavaCG2ConstantTypeEnum getFromType(String type) {
        for (JavaCG2ConstantTypeEnum constantTypeEnum : JavaCG2ConstantTypeEnum.values()) {
            if (constantTypeEnum.getType().equals(type)) {
                return constantTypeEnum;
            }
        }
        return JavaCG2ConstantTypeEnum.CONSTTE_ILLEGAL;
    }

    /**
     * 判断指定的类型是否为常量类型
     *
     * @param type
     * @return
     */
    public static boolean isConstantType(String type) {
        return JavaCG2ConstantTypeEnum.CONSTTE_ILLEGAL != getFromType(type);
    }

    /**
     * 判断指定的类型是否为基本类型
     *
     * @param type
     * @return
     */
    public static boolean isPrimitiveType(String type) {
        return getFromType(type).isPrimitive();
    }

    @Override
    public String toString() {
        return type;
    }
}
