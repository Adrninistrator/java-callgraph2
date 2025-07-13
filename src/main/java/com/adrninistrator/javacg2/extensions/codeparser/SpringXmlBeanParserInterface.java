package com.adrninistrator.javacg2.extensions.codeparser;

import com.adrninistrator.javacg2.dto.spring.SpringBeanInXml;

import java.util.List;

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
     * 获取XML中定义的Spring Bean信息
     *
     * @return
     */
    List<SpringBeanInXml> getBeanInXml();
}
