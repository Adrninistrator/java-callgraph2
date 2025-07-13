package com.adrninistrator.javacg2.dto.spring;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/6/18
 * @description: Java代码中定义的Spring Bean信息
 */
public class SpringBeanInJava {

    // Spring Bean可能的类名
    private List<String> classNameList;

    // Spring Bean定义时使用的注解类名
    private String annotationClassName;

    // Spring Bean定义时所在的类名
    private String defineClassName;

    public List<String> getClassNameList() {
        return classNameList;
    }

    public void setClassNameList(List<String> classNameList) {
        this.classNameList = classNameList;
    }

    public String getAnnotationClassName() {
        return annotationClassName;
    }

    public void setAnnotationClassName(String annotationClassName) {
        this.annotationClassName = annotationClassName;
    }

    public String getDefineClassName() {
        return defineClassName;
    }

    public void setDefineClassName(String defineClassName) {
        this.defineClassName = defineClassName;
    }
}
