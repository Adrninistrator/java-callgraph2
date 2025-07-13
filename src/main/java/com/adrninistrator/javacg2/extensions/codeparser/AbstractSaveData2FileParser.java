package com.adrninistrator.javacg2.extensions.codeparser;

import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description: 解析并将结果保存在文件的类（对jar文件中的其他文件解析）
 */
public abstract class AbstractSaveData2FileParser implements JarEntryOtherFileParser {
    private static final Logger logger = LoggerFactory.getLogger(AbstractSaveData2FileParser.class);

    // 保存写文件的Writer类，一个
    private Writer writer;

    /**
     * 保存写文件的Writer类，多个
     * key  文件名
     * value    对应写文件的Writer类
     */
    private Map<String, Writer> writerMap;

    /**
     * 返回当前生成的文件名
     * 需要在子类中实现该方法
     *
     * @return
     */
    public String chooseFileName() {
        return null;
    }

    /**
     * 返回当前生成的文件名，多个
     * 需要在子类中实现该方法
     *
     * @return
     */
    public String[] chooseFileNames() {
        return null;
    }

    /**
     * 初始化
     *
     * @param outputDirPath 输出文件保存路径
     * @param outputDirPath 输出文件后缀
     */
    public boolean init(String outputDirPath, String outputFileExt) {
        try {
            String[] fileNames = chooseFileNames();
            if (fileNames != null) {
                // 输出多个文件
                writerMap = new HashMap<>();
                for (String fileName : fileNames) {
                    if (writerMap.containsKey(fileName)) {
                        logger.error("当前类定义了重复的文件名 {} {}", this.getClass().getName(), fileName);
                        return false;
                    }
                    String filePath = JavaCG2FileUtil.genFilePath(outputDirPath, fileName, outputFileExt);
                    Writer tmpWriter = JavaCG2FileUtil.genBufferedWriter(filePath);
                    writerMap.put(fileName, tmpWriter);
                }
                return true;
            }
            String fileName = chooseFileName();
            if (StringUtils.isNotBlank(fileName)) {
                // 输出一个文件
                String filePath = JavaCG2FileUtil.genFilePath(outputDirPath, fileName, outputFileExt);
                writer = JavaCG2FileUtil.genBufferedWriter(filePath);
                return true;
            }
            logger.error("当前方法未指定需要处理的文件名 {}", this.getClass().getName());
            return false;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 将数据写入文件，仅写一个文件时使用
     *
     * @param data
     * @throws IOException
     */
    public void writeData2File(String... data) throws IOException {
        JavaCG2FileUtil.write2FileWithTab(writer, data);
    }

    /**
     * 将数据写入文件，写入多个文件时使用
     *
     * @param fileName
     * @param data
     * @throws IOException
     */
    public void writeData2Files(String fileName, String... data) throws IOException {
        Writer tmpWriter = writerMap.get(fileName);
        JavaCG2FileUtil.write2FileWithTab(tmpWriter, data);
    }

    /**
     * 关闭
     */
    public void close() {
        try {
            if (writerMap != null) {
                for (Writer tmpWriter : writerMap.values()) {
                    tmpWriter.close();
                }
            }
            if (writer != null) {
                writer.close();
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}
