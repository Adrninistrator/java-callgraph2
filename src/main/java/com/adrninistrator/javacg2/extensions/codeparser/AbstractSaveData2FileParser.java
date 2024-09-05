package com.adrninistrator.javacg2.extensions.codeparser;

import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Writer;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description: 解析并将结果保存在文件的类（对jar包中的其他文件解析）
 */
public abstract class AbstractSaveData2FileParser implements JarEntryOtherFileParser {
    private static final Logger logger = LoggerFactory.getLogger(AbstractSaveData2FileParser.class);

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
            writer = JavaCG2FileUtil.genBufferedWriter(outputFilePath);
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
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
            logger.error("error ", e);
        }
    }
}
