package com.adrninistrator.javacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/7/2
 * @description: 方法调用可能的信息类型枚举
 */
public enum JavaCGMethodCallInfoTypeEnum {
    MCIT_TYPE("t", "类型"),
    MCIT_VALUE("v", "值"),
    MCIT_BASE64_VALUE("bv", "base64编码的值"),
    MCIT_STATIC_FIELD("sf", "静态字段"),
    MCIT_STATIC_FIELD_METHOD_CALL("sfm", "静态字段的方法调用"),
    MCIT_NAME_OF_FIELD("nof", "字段的名称"),
    MCIT_NAME_OF_VARIABLE("nov", "变量的名称"),
    MCIT_METHOD_CALL_RETURN_CALL_ID("mcrci", "方法调用返回的call_id"),
    MCIT_METHOD_ARG_SEQ("mas", "方法参数的序号"),
    MCIT_METHOD_CALL_RETURN_CALL_ID_EQC("mcrci_eqc", "等值转换前的方法调用返回的call_id"),
    MCIT_METHOD_ARG_SEQ_EQC("mas_eqc", "等值转换前的方法参数的序号"),
    MCIT_METHOD_CATCH_EXCEPTION_FROM_OFFSET("mcefo", "方法catch的异常对象对应的catch代码块开始指令偏移量"),
    MCIT_ILLEGAL("_illegal", "非法类型"),
    ;

    private final String type;
    private final String desc;

    JavaCGMethodCallInfoTypeEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public static JavaCGMethodCallInfoTypeEnum getFromType(String type) {
        for (JavaCGMethodCallInfoTypeEnum javaCGMethodCallInfoTypeEnum : JavaCGMethodCallInfoTypeEnum.values()) {
            if (javaCGMethodCallInfoTypeEnum.getType().equals(type)) {
                return javaCGMethodCallInfoTypeEnum;
            }
        }
        return MCIT_ILLEGAL;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }
}
