package com.adrninistrator.javacg2.dto.output;

import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/5
 * @description: java-callgraph2处理结果信息
 */
public class JavaCG2OutputInfo {

    // 保存当前的输出目录路径
    private final String outputDirPath;

    // 保存输出文件后缀名
    private final String outputFileExt;

    /*
        保存其他文件的信息
        key
            文件名，不需要指定文件后缀
        value
            文件路径
     */
    private final Map<String, String> otherFilePathMap = new HashMap<>();

    // 栈桢信息快照数量超过允许的最大数量的方法Set
    private final Set<String> frameSnapshotNumExceedMethodSet = new HashSet<>();

    public JavaCG2OutputInfo(String outputDirPath, String outputFileExt) {
        // 路径后面增加路径分隔符
        this.outputDirPath = JavaCG2FileUtil.addSeparator4FilePath(outputDirPath);
        this.outputFileExt = outputFileExt;
    }

    /**
     * 增加其他文件信息
     *
     * @param fileName 文件名，不需要指定文件后缀
     */
    public void addOtherFileInfo(String fileName) {
        String filePath = JavaCG2FileUtil.genFilePath(outputDirPath, fileName, outputFileExt);
        otherFilePathMap.put(fileName, filePath);
    }

    /**
     * 获取其他文件路径
     *
     * @param fileName
     * @return
     */
    public String getOtherFilePath(String fileName) {
        return otherFilePathMap.get(fileName);
    }

    /**
     * 获取其他文件名集合
     *
     * @return
     */
    public Set<String> getOtherFileNameSet() {
        return otherFilePathMap.keySet();
    }

    /**
     * 获取当前生成文件的目录
     *
     * @return
     */
    public String getOutputDirPath() {
        return outputDirPath;
    }

    /**
     * 获取指定类型的生成文件的路径
     *
     * @param javaCG2OutPutFileTypeEnum
     * @return
     */
    public String getMainFilePath(JavaCG2OutPutFileTypeEnum javaCG2OutPutFileTypeEnum) {
        return JavaCG2FileUtil.genFilePath(outputDirPath, javaCG2OutPutFileTypeEnum.getFileName(), outputFileExt);
    }

    public void addFrameSnapshotNumExceedMethod(String frameSnapshotNumExceedMethod) {
        frameSnapshotNumExceedMethodSet.add(frameSnapshotNumExceedMethod);
    }

    public void addFrameSnapshotNumExceedMethodSet(Set<String> frameSnapshotNumExceedMethodSet) {
        this.frameSnapshotNumExceedMethodSet.addAll(frameSnapshotNumExceedMethodSet);
    }

    public Set<String> getFrameSnapshotNumExceedMethodSetReadOnly() {
        return Collections.unmodifiableSet(frameSnapshotNumExceedMethodSet);
    }

    public String getOutputFileExt() {
        return outputFileExt;
    }
}
