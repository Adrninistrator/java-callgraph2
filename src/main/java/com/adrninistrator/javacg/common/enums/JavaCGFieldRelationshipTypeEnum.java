package com.adrninistrator.javacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/8/6
 * @description: 通过get/set方法关联的字段关联类型
 */
public enum JavaCGFieldRelationshipTypeEnum {
    FRTE_DIRECTLY("DI", "字段直接赋值"),
    FRTE_DIRECTLY_EQUIVALENT_CONVERSION("DI_EQC", "字段经过等值转换直接赋值"),
    FRTE_BEAN_UTIL("BU", "通过BeanUtil等方法拷贝赋值"),
    FRTE_MYBATIS_MAPPER_ARG_DB("MMAD", "MyBatis Mapper方法参数对应数据库字段"),
    FRTE_METHOD_CALL_PASSED("MCP", "通过方法调用传递的字段赋值"),
    FRTE_METHOD_CALL_PASSED_EQC("MCP_EQC", "字段经过等值转换，通过方法调用传递的字段赋值"),
    ;

    private final String type;
    private final String desc;

    JavaCGFieldRelationshipTypeEnum(String type, String desc) {
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
