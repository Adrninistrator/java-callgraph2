package com.adrninistrator.javacg.conf;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description: 配置参数类
 */
public class JavaCGConfInfo {

    // 需要处理的jar包或目录
    private List<String> jarDirList;

    // 需要处理的的包名
    private Set<String> needHandlePackageSet;

    // 合并jar包、目录时需要处理的文件类型
    private Set<String> jarDirMergeFileTypeSet;

    // 在处理jar包中的文件时，文件路径包含当前Set中的关键字时就忽略
    private Set<String> ignoreJarFileKeywordSet;

    // 在处理jar包中的文件时，文件名在当前Set中时就忽略
    private Set<String> ignoreJarFileNameSet;

    /*
        在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法
        外层key   类名
        外层value
            内层key   方法名
            内层value 与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号
     */
    private Map<String, Map<String, Integer>> frEqConversionMethodMap;

    // 处理方法调用时是否解析可能的类型与值
    private boolean parseMethodCallTypeValue;

    // 处理类的方法前是否需要先解析构造函数以非静态字段可能的类型
    private boolean firstParseInitMethodType;

    // 是否需要分析dto的字段之间的关联关系
    private boolean analyseFieldRelationship;

    // 处理方法出现异常时，是否要继续
    private boolean continueWhenError;

    // 记录方法分析耗时的开关
    private boolean logMethodSpendTime;

    // 生成文件的根目录
    private String outputRootPath;

    // 当前使用的生成文件的目录
    private String usedOutputDirPath;

    // 生成文件后缀名
    private String outputFileExt;

    public List<String> getJarDirList() {
        return jarDirList;
    }

    public void setJarDirList(List<String> jarDirList) {
        this.jarDirList = jarDirList;
    }

    public Set<String> getNeedHandlePackageSet() {
        return needHandlePackageSet;
    }

    public void setNeedHandlePackageSet(Set<String> needHandlePackageSet) {
        this.needHandlePackageSet = needHandlePackageSet;
    }

    public Set<String> getJarDirMergeFileTypeSet() {
        return jarDirMergeFileTypeSet;
    }

    public void setJarDirMergeFileTypeSet(Set<String> jarDirMergeFileTypeSet) {
        this.jarDirMergeFileTypeSet = jarDirMergeFileTypeSet;
    }

    public Set<String> getIgnoreJarFileKeywordSet() {
        return ignoreJarFileKeywordSet;
    }

    public void setIgnoreJarFileKeywordSet(Set<String> ignoreJarFileKeywordSet) {
        this.ignoreJarFileKeywordSet = ignoreJarFileKeywordSet;
    }

    public Set<String> getIgnoreJarFileNameSet() {
        return ignoreJarFileNameSet;
    }

    public void setIgnoreJarFileNameSet(Set<String> ignoreJarFileNameSet) {
        this.ignoreJarFileNameSet = ignoreJarFileNameSet;
    }

    public Map<String, Map<String, Integer>> getFrEqConversionMethodMap() {
        return frEqConversionMethodMap;
    }

    public void setFrEqConversionMethodMap(Map<String, Map<String, Integer>> frEqConversionMethodMap) {
        this.frEqConversionMethodMap = frEqConversionMethodMap;
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

    public boolean isContinueWhenError() {
        return continueWhenError;
    }

    public void setContinueWhenError(boolean continueWhenError) {
        this.continueWhenError = continueWhenError;
    }

    public boolean isLogMethodSpendTime() {
        return logMethodSpendTime;
    }

    public void setLogMethodSpendTime(boolean logMethodSpendTime) {
        this.logMethodSpendTime = logMethodSpendTime;
    }

    public String getOutputRootPath() {
        return outputRootPath;
    }

    public void setOutputRootPath(String outputRootPath) {
        this.outputRootPath = outputRootPath;
    }

    public String getUsedOutputDirPath() {
        return usedOutputDirPath;
    }

    public void setUsedOutputDirPath(String usedOutputDirPath) {
        this.usedOutputDirPath = usedOutputDirPath;
    }

    public String getOutputFileExt() {
        return outputFileExt;
    }

    public void setOutputFileExt(String outputFileExt) {
        this.outputFileExt = outputFileExt;
    }
}
