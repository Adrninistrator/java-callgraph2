package com.adrninistrator.javacg.dto.output;

import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/5
 * @description: java-callgraph2处理结果信息
 */
public class JavaCGOutputInfo {

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

    public JavaCGOutputInfo(String outputDirPath, String outputFileExt) {
        // 路径后面增加路径分隔符
        this.outputDirPath = JavaCGUtil.addSeparator4FilePath(outputDirPath);
        this.outputFileExt = outputFileExt;
    }

    /**
     * 增加其他文件信息，返回对应的文件路径
     *
     * @param fileName 文件名，不需要指定文件后缀
     * @return 文件路径
     */
    public String addOtherFileInfo(String fileName) {
        String filePath = outputDirPath + fileName + outputFileExt;
        otherFilePathMap.put(fileName, filePath);
        return filePath;
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
     * @param javaCGOutPutFileTypeEnum
     * @return
     */
    public String getMainFilePath(JavaCGOutPutFileTypeEnum javaCGOutPutFileTypeEnum) {
        return outputDirPath + javaCGOutPutFileTypeEnum.getFileName() + outputFileExt;
    }
}
