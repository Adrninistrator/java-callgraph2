package com.adrninistrator.javacg2.common.enums;

import com.adrninistrator.javacg2.common.JavaCG2Constants;

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

    public static String getAllInfoOneLine() {
        return getAllInfo(true);
    }

    public static String getAllInfoMultiLine() {
        return getAllInfo(false);
    }

    private static String getAllInfo(boolean oneLine) {
        StringBuilder stringBuilder = new StringBuilder();
        for (JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnum : JavaCG2CalleeRawActualEnum.values()) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append(oneLine ? JavaCG2Constants.FLAG_TAB : JavaCG2Constants.NEW_LINE_WINDOWS);
            }
            stringBuilder.append(javaCG2CalleeRawActualEnum.getType()).append(": ").append(javaCG2CalleeRawActualEnum.getDesc());
        }
        return stringBuilder.toString();
    }
}
