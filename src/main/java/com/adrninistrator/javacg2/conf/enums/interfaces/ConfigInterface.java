package com.adrninistrator.javacg2.conf.enums.interfaces;

/**
 * @author adrninistrator
 * @date 2022/11/14
 * @description: 配置枚举继承的接口
 */
public interface ConfigInterface {

    // 获取枚举常量名称
    String getEnumConstantName();

    // 获取key
    String getKey();

    // 获取描述
    String[] getDescriptions();

    // 获取配置用于打印的信息
    String getConfigPrintInfo();
}
