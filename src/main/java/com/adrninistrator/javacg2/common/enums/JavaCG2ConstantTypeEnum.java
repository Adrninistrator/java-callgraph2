package com.adrninistrator.javacg2.common.enums;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量类型枚举
 */
public enum JavaCG2ConstantTypeEnum {
    // 以下都使用实际的类型，用于进行比较
    CONSTTE_NULL("null", false),
    CONSTTE_INT(int.class.getName(), true),
    CONSTTE_LONG(long.class.getName(), true),
    CONSTTE_FLOAT(float.class.getName(), true),
    CONSTTE_DOUBLE(double.class.getName(), true),
    CONSTTE_BYTE(byte.class.getName(), true),
    CONSTTE_CHAR(char.class.getName(), true),
    CONSTTE_SHORT(short.class.getName(), true),
    CONSTTE_STRING(String.class.getName(), false),
    CONSTTE_BOOLEAN(boolean.class.getName(), true),
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
