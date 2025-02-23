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

    /*
        在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法
        外层key   类名
        外层value
            内层key   方法名
            内层value 与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号
     */
    private Map<String, Map<String, Integer>> frEqConversionMethodMap;

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

    public Map<String, Map<String, Integer>> getFrEqConversionMethodMap() {
        return frEqConversionMethodMap;
    }

    public void setFrEqConversionMethodMap(Map<String, Map<String, Integer>> frEqConversionMethodMap) {
        this.frEqConversionMethodMap = frEqConversionMethodMap;
    }
}
