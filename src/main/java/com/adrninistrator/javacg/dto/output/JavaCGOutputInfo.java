package com.adrninistrator.javacg.dto.output;

import com.adrninistrator.javacg.common.JavaCGConstants;

import java.io.File;
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
        if (outputDirPath.endsWith(File.separator)) {
            this.outputDirPath = outputDirPath;
        } else {
            this.outputDirPath = outputDirPath + File.separator;
        }
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

    //
    public String getOutputDirPath() {
        return outputDirPath;
    }

    public String getJarInfoOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_JAR_INFO + outputFileExt;
    }

    public String getClassNameOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_CLASS_NAME + outputFileExt;
    }

    public String getMethodCallOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_METHOD_CALL + outputFileExt;
    }

    public String getLambdaMethodInfoOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_LAMBDA_METHOD_INFO + outputFileExt;
    }

    public String getClassAnnotationOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_CLASS_ANNOTATION + outputFileExt;
    }

    public String getMethodAnnotationOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_METHOD_ANNOTATION + outputFileExt;
    }

    public String getMethodLineNumberOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_METHOD_LINE_NUMBER + outputFileExt;
    }

    public String getMethodCallInfoOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_METHOD_CALL_INFO + outputFileExt;
    }

    public String getClassInfoOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_CLASS_INFO + outputFileExt;
    }

    public String getMethodInfoOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_METHOD_INFO + outputFileExt;
    }

    public String getExtendsImplOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_EXTENDS_IMPL + outputFileExt;
    }

    public String getSpringBeanOutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_SPRING_BEAN + outputFileExt;
    }

    public String getClassSignatureEI1OutputFilePath() {
        return outputDirPath + JavaCGConstants.FILE_CLASS_SIGNATURE_EI1 + outputFileExt;
    }

    public String getMethodArgGenericsTypeFilePath() {
        return outputDirPath + JavaCGConstants.FILE_METHOD_ARG_GENERICS_TYPE + outputFileExt;
    }

    public String getInnerClassFilePath() {
        return outputDirPath + JavaCGConstants.FILE_INNER_CLASS + outputFileExt;
    }
}
