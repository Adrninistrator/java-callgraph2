package com.adrninistrator.javacg.writer;

import com.adrninistrator.javacg.util.JavaCGFileUtil;

import java.io.Closeable;
import java.io.IOException;
import java.io.Writer;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description: 包装后的Writer，若未写文件则不创建文件
 */
public class WriterSupportSkip implements Closeable {

    private final String filePath;

    private Writer writer;

    public WriterSupportSkip(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void close() throws IOException {
        if (writer != null) {
            writer.close();
        }
    }

    public void write(String data) throws IOException {
        if (writer == null) {
            writer = JavaCGFileUtil.genBufferedWriter(filePath);
        }
        writer.write(data);
    }
}
