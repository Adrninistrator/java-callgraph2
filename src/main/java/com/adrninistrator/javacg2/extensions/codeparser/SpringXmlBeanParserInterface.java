package com.adrninistrator.javacg2.extensions.codeparser;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description: 对Spring XML文件中的bean解析的接口
 */
public interface SpringXmlBeanParserInterface extends JarEntryOtherFileParser {

    /**
     * 根据Spring Bean的id获取对应的类名
     *
     * @param beanId
     * @return
     */
    String getBeanClass(String beanId);

    /**
     * 获取Spring Bean对应的Map
     * key Bean名称
     * value Bean的类名
     *
     * @return
     */
    Map<String, String> getBeanMap();
}
