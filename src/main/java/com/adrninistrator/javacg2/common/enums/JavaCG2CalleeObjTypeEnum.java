package com.adrninistrator.javacg2.common.enums;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description: 被调用类型枚举
 */
public enum JavaCG2CalleeObjTypeEnum {
    COTE_THIS("t", "调用当前实例的方法"),
    COTE_STATIC_FIELD("sf", "调用静态字段的方法"),
    COTE_FIELD("f", "调用字段的方法"),
    COTE_VARIABLE("v", "调用其他变量的方法"),
    ;

    private final String type;
    private final String desc;

    JavaCG2CalleeObjTypeEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }
}
