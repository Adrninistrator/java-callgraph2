package com.adrninistrator.javacg2.common;

/**
 * @author adrninistrator
 * @date 2025/1/29
 * @description: 打印配置参数时使用的常量
 */
public class JavaCG2ConfigPrintConstants {

    public static final String CONFIG_FLAG_FILE_KEY = "配置文件名称";
    public static final String CONFIG_FLAG_FILE_ENUM_CLASS_NAME = "配置文件枚举类名";
    public static final String CONFIG_FLAG_FILE_ENUM_CONSTANT_NAME = "配置文件枚举常量名";
    public static final String CONFIG_FLAG_FILE_ENUM_CLASS_CONSTANT_NAME = "配置文件枚举类名与常量名";
    public static final String CONFIG_FLAG_FILE_DESC = "配置文件说明";
    public static final String CONFIG_FLAG_CONF_KEY = "参数名称";
    public static final String CONFIG_FLAG_CONF_DESC = "参数说明";
    public static final String CONFIG_FLAG_CONF_TYPE = "参数类型";
    public static final String CONFIG_FLAG_CONF_NOT_BLANK = "参数值是否必填";
    public static final String CONFIG_FLAG_CONF_ENUM_NAME = "参数枚举名";
    public static final String CONFIG_FLAG_CONF_DEFAULT_VALUE = "参数默认值";
    public static final String CONFIG_FLAG_CONF_VALUE = "当前使用参数值";
    public static final String CONFIG_FLAG_CONF_USED = "当前有使用的";
    public static final String CONFIG_FLAG_CONF_MAIN = "主要的配置文件参数";
    public static final String CONFIG_FLAG_CONF_LIST = "区分顺序的其他配置参数";
    public static final String CONFIG_FLAG_CONF_SET = "不区分顺序的其他配置参数";
    public static final String CONFIG_FLAG_CONF_EL = "表达式配置参数";

    public static final String CONFIG_FLAG_EL_ALLOWED_VARIABLES = "允许使用的变量";
    public static final String CONFIG_FLAG_EL_VARIABLE_NAME = "变量名称";
    public static final String CONFIG_FLAG_EL_VARIABLE_TYPE = "变量类型";
    public static final String CONFIG_FLAG_EL_VARIABLE_DESC = "变量描述";
    public static final String CONFIG_FLAG_EL_VARIABLE_VALUE_EXAMPLE = "变量值示例";


    private JavaCG2ConfigPrintConstants() {
        throw new IllegalStateException("illegal");
    }
}
