package com.adrninistrator.javacg.extensions.code_parser;

import com.adrninistrator.javacg.util.JavaCGFileUtil;

import java.io.Writer;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description: 解析并将结果保存在文件的类（对jar包中的其他文件解析）
 */
public abstract class AbstractSaveData2FileParser implements JarEntryOtherFileParser {

    protected Writer writer;

    /**
     * 返回当前生成的文件名
     *
     * @return
     */
    public abstract String chooseFileName();

    /**
     * 初始化
     *
     * @param outputFilePath 输出文件路径
     */
    public boolean init(String outputFilePath) {
        try {
            writer = JavaCGFileUtil.genBufferedWriter(outputFilePath);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * 关闭
     */
    public void close() {
        try {
            writer.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
