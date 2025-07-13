package com.adrninistrator.javacg2.dto.jar;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2025/7/6
 * @description: 合并jar文件的结果
 */
public class MergeJarResult {

    // 合并后的jar文件
    private File mergeJarFile;

    // 合并生成的fat jar文件
    private File fatJarFile;

    public static MergeJarResult genOne(File mergeJarFile) {
        MergeJarResult mergeJarResult = new MergeJarResult();
        mergeJarResult.setMergeJarFile(mergeJarFile);
        return mergeJarResult;
    }

    public static MergeJarResult genTwo(File mergeJarFile, File fatJarFile) {
        MergeJarResult mergeJarResult = new MergeJarResult();
        mergeJarResult.setMergeJarFile(mergeJarFile);
        mergeJarResult.setFatJarFile(fatJarFile);
        return mergeJarResult;
    }

    public File getMergeJarFile() {
        return mergeJarFile;
    }

    public void setMergeJarFile(File mergeJarFile) {
        this.mergeJarFile = mergeJarFile;
    }

    public File getFatJarFile() {
        return fatJarFile;
    }

    public void setFatJarFile(File fatJarFile) {
        this.fatJarFile = fatJarFile;
    }
}
