package com.adrninistrator.javacg2.common.enums;

/**
 * @author adrninistrator
 * @date 2023/3/12
 * @description:
 */
public enum JavaCG2YesNoEnum {
    YES("1", 1, "是"),
    NO("0", 0, "否"),
    NOT_SURE("2", 2, "不确定"),
    ;

    private final String strValue;
    private final int intValue;
    private final String desc;

    JavaCG2YesNoEnum(String strValue, int intValue, String desc) {
        this.strValue = strValue;
        this.intValue = intValue;
        this.desc = desc;
    }

    public static String parseStrValue(boolean value) {
        return value ? YES.strValue : NO.strValue;
    }

    public static int parseIntValue(boolean value) {
        return value ? YES.intValue : NO.intValue;
    }

    public static String parseDesc(boolean value) {
        return value ? YES.desc : NO.desc;
    }

    public static boolean isYes(String value) {
        return YES.strValue.equals(value);
    }

    public static boolean isYes(int value) {
        return YES.intValue == value;
    }

    public static boolean isYesDesc(String desc) {
        return YES.desc.equals(desc);
    }

    public static boolean isNotSure(String value) {
        return NOT_SURE.strValue.equals(value);
    }

    public static boolean isNotSure(int value) {
        return NOT_SURE.intValue == value;
    }

    public static boolean isNotSureDesc(String desc) {
        return NOT_SURE.desc.equals(desc);
    }

    public String getStrValue() {
        return strValue;
    }

    public int getIntValue() {
        return intValue;
    }

    public String getDesc() {
        return desc;
    }
}
