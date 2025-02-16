package com.adrninistrator.javacg2.dto.jar;

/**
 * @author adrninistrator
 * @date 2025/1/27
 * @description: jar/war文件及其中的jar文件
 */
public class OuterInnerJarPath {

    // 外层jar/war文件路径
    private String outerJarPath;

    // 内层jar文件路径
    private String innerJarPath;

    public String getOuterJarPath() {
        return outerJarPath;
    }

    public void setOuterJarPath(String outerJarPath) {
        this.outerJarPath = outerJarPath;
    }

    public String getInnerJarPath() {
        return innerJarPath;
    }

    public void setInnerJarPath(String innerJarPath) {
        this.innerJarPath = innerJarPath;
    }
}
