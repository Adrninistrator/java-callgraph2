package com.adrninistrator.javacg2.common.enums;

/**
 * @author adrninistrator
 * @date 2025/3/23
 * @description: 选择涉及多态的被调用的类型，使用原始的还是实际的
 */
public enum JavaCG2CalleeRawActualEnum {
    CRAE_ONLY_RAW("only_raw", "记录一条方法调用关系，被调用类型使用：原始类型"),
    CRAE_ONLY_ACTUAL("only_actual", "记录一条方法调用关系，被调用类型使用：实际类型"),
    CRAE_RAW_ACTUAL("raw_actual", "记录两条方法调用关系，被调用类型分别使用：原始类型、实际类型"),
    ;

    private final String type;
    private final String desc;

    JavaCG2CalleeRawActualEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }

    public static JavaCG2CalleeRawActualEnum getFromType(String type) {
        for (JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnum : JavaCG2CalleeRawActualEnum.values()) {
            if (javaCG2CalleeRawActualEnum.getType().equals(type)) {
                return javaCG2CalleeRawActualEnum;
            }
        }
        return null;
    }

    public static String getAllInfo() {
        StringBuilder stringBuilder = new StringBuilder();
        for (JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnum : JavaCG2CalleeRawActualEnum.values()) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append("\t");
            }
            stringBuilder.append(javaCG2CalleeRawActualEnum.getType()).append(": ").append(javaCG2CalleeRawActualEnum.getDesc());
        }
        return stringBuilder.toString();
    }
}
