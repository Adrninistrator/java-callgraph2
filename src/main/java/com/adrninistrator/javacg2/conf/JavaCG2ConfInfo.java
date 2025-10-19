package com.adrninistrator.javacg2.conf;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description: 配置参数类
 */
public class JavaCG2ConfInfo {

    // 当前使用的生成文件的目录
    private String outputDirPath;

    // 处理方法调用时是否解析可能的类型与值
    private boolean parseMethodCallTypeValue;

    // 处理类的方法前是否需要先解析构造函数以非静态字段可能的类型
    private boolean firstParseInitMethodType;

    // 是否需要分析dto的字段之间的关联关系
    private boolean analyseFieldRelationship;

    // 解析方法调用时，通过new创建的被调用类型是否处理原始类型
    private boolean handleCalleeNewRaw;

    // 解析方法调用时，通过new创建的被调用类型是否处理实际类型
    private boolean handleCalleeNewActual;

    // 解析方法调用时，被调用对象为Spring Bean，是否处理原始类型（支持字段注入）
    private boolean handleCalleeSpringBeanRaw;

    // 解析方法调用时，被调用对象为Spring Bean，是否处理实际类型（支持字段注入）
    private boolean handleCalleeSpringBeanActual;

    /*
        在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法
        外层key   类名
        外层value
            内层key   方法名
            内层value 与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号
     */
    private Map<String, Map<String, Integer>> frEqConversionMethodMap;

    // 是否使用Jar兼容性检查模式，仅解析类、方法、字段等基础信息
    private boolean parseJarCompatibilityMode;

    // 是否仅解析类信息
    private boolean parseOnlyClassMode;

    public String getOutputDirPath() {
        return outputDirPath;
    }

    public void setOutputDirPath(String outputDirPath) {
        this.outputDirPath = outputDirPath;
    }

    public boolean isParseMethodCallTypeValue() {
        return parseMethodCallTypeValue;
    }

    public void setParseMethodCallTypeValue(boolean parseMethodCallTypeValue) {
        this.parseMethodCallTypeValue = parseMethodCallTypeValue;
    }

    public boolean isFirstParseInitMethodType() {
        return firstParseInitMethodType;
    }

    public void setFirstParseInitMethodType(boolean firstParseInitMethodType) {
        this.firstParseInitMethodType = firstParseInitMethodType;
    }

    public boolean isAnalyseFieldRelationship() {
        return analyseFieldRelationship;
    }

    public void setAnalyseFieldRelationship(boolean analyseFieldRelationship) {
        this.analyseFieldRelationship = analyseFieldRelationship;
    }

    public boolean isHandleCalleeNewRaw() {
        return handleCalleeNewRaw;
    }

    public void setHandleCalleeNewRaw(boolean handleCalleeNewRaw) {
        this.handleCalleeNewRaw = handleCalleeNewRaw;
    }

    public boolean isHandleCalleeNewActual() {
        return handleCalleeNewActual;
    }

    public void setHandleCalleeNewActual(boolean handleCalleeNewActual) {
        this.handleCalleeNewActual = handleCalleeNewActual;
    }

    public boolean isHandleCalleeSpringBeanRaw() {
        return handleCalleeSpringBeanRaw;
    }

    public void setHandleCalleeSpringBeanRaw(boolean handleCalleeSpringBeanRaw) {
        this.handleCalleeSpringBeanRaw = handleCalleeSpringBeanRaw;
    }

    public boolean isHandleCalleeSpringBeanActual() {
        return handleCalleeSpringBeanActual;
    }

    public void setHandleCalleeSpringBeanActual(boolean handleCalleeSpringBeanActual) {
        this.handleCalleeSpringBeanActual = handleCalleeSpringBeanActual;
    }

    public Map<String, Map<String, Integer>> getFrEqConversionMethodMap() {
        return frEqConversionMethodMap;
    }

    public void setFrEqConversionMethodMap(Map<String, Map<String, Integer>> frEqConversionMethodMap) {
        this.frEqConversionMethodMap = frEqConversionMethodMap;
    }

    public boolean isParseJarCompatibilityMode() {
        return parseJarCompatibilityMode;
    }

    public void setParseJarCompatibilityMode(boolean parseJarCompatibilityMode) {
        this.parseJarCompatibilityMode = parseJarCompatibilityMode;
    }

    public boolean isParseOnlyClassMode() {
        return parseOnlyClassMode;
    }

    public void setParseOnlyClassMode(boolean parseOnlyClassMode) {
        this.parseOnlyClassMode = parseOnlyClassMode;
    }
}
