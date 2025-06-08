package com.adrninistrator.javacg2.conf.enums.interfaces;

/**
 * @author adrninistrator
 * @date 2025/1/30
 * @description: List与Set格式的其他枚举配置实现的接口
 */
public interface OtherConfigInterface extends ConfigInterface {

    // 配置参数属于Set类型还是List类型
    boolean isSetOrList();

    // 通过key获取对应的配置枚举
    OtherConfigInterface getFromKey(String key);

    // 默认值
    String[] getDefaultValues();
}
