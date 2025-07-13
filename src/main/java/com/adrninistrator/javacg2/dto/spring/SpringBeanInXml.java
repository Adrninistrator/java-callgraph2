package com.adrninistrator.javacg2.dto.spring;

/**
 * @author adrninistrator
 * @date 2025/6/18
 * @description: XML文件中定义的Spring Bean信息
 */
public class SpringBeanInXml {

    // Spring Bean名称
    private String springBeanName;

    // Spring Bean类名
    private String className;

    // XML文件路径
    private String xmlFilePath;

    public String getSpringBeanName() {
        return springBeanName;
    }

    public void setSpringBeanName(String springBeanName) {
        this.springBeanName = springBeanName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getXmlFilePath() {
        return xmlFilePath;
    }

    public void setXmlFilePath(String xmlFilePath) {
        this.xmlFilePath = xmlFilePath;
    }
}
