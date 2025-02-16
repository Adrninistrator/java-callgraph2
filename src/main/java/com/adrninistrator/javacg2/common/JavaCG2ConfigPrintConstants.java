package com.adrninistrator.javacg2.common;

/**
 * @author adrninistrator
 * @date 2025/1/29
 * @description: 打印配置信息时使用的常量
 */
public class JavaCG2ConfigPrintConstants {

    public static final String MAIN_CONFIG = "主要的配置信息";
    public static final String CONFIG_FLAG_FILE_KEY = "配置文件名称";
    public static final String CONFIG_FLAG_FILE_ENUM_CLASS = "配置文件枚举类名";
    public static final String CONFIG_FLAG_FILE_ENUM_CLASS_AND_NAME = "配置文件枚举类名与枚举名";
    public static final String CONFIG_FLAG_FILE_DESC = "配置文件说明";
    public static final String CONFIG_FLAG_CONF_KEY = "参数名称";
    public static final String CONFIG_FLAG_CONF_ENUM_NAME = "参数枚举名";
    public static final String CONFIG_FLAG_CONF_DESC = "参数说明";
    public static final String CONFIG_FLAG_CONF_VALUE = "参数值";
    public static final String CONFIG_FLAG_CONF_LIST = "区分顺序的其他配置信息";
    public static final String CONFIG_FLAG_CONF_SET = "不区分顺序的其他配置信息";

    private JavaCG2ConfigPrintConstants() {
        throw new IllegalStateException("illegal");
    }
}
