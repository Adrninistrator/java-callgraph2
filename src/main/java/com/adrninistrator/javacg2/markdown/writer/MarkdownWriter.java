package com.adrninistrator.javacg2.markdown.writer;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.markdown.MarkdownConstants;
import com.adrninistrator.javacg2.markdown.enums.MDCodeBlockTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/27
 * @description: markdown格式处理类
 */
public class MarkdownWriter implements AutoCloseable {
    private static final Logger logger = LoggerFactory.getLogger(MarkdownWriter.class);

    // 标题是否显示数字序号，如1. 2.3.
    private final boolean showTitleSeq;

    private final BufferedWriter writer;

    // 上一次添加的标题级别
    private int lastAddedTitleLevel;

    // 标题级别及对应的序号
    private Map<Integer, Integer> titleLevelSeqMap;

    // 代码块标志数量
    private int codeBlockFlagNum = 0;

    // 是否有写入指定数据的标志
    private boolean writeData = false;

    // 正在写入表格的列数
    private int tableColumnNum = 0;

    /**
     * @param mdFilePath   md文件路径
     * @param showTitleSeq 标题是否显示数字序号，如1. 2.3.
     * @throws FileNotFoundException
     */
    public MarkdownWriter(String mdFilePath, boolean showTitleSeq) throws FileNotFoundException {
        this.showTitleSeq = showTitleSeq;
        writer = JavaCG2FileUtil.genBufferedWriter(mdFilePath);
        if (showTitleSeq) {
            titleLevelSeqMap = new HashMap<>();
        }
    }

    /**
     * 增加标题
     *
     * @param level 标题级别
     * @param title 标题内容
     * @throws IOException
     */
    public void addTitle(int level, String title) throws IOException {
        if (level <= 0) {
            throw new JavaCG2RuntimeException("标题级别需要大于0");
        }

        StringBuilder data = new StringBuilder(StringUtils.repeat(MarkdownConstants.FLAG_TITLE, level))
                .append(MarkdownConstants.FLAG_SPACE);
        if (showTitleSeq) {
            // 生成标题序号
            data.append(genTitleSeq(level)).append(MarkdownConstants.FLAG_SPACE);
        }

        data.append(title).append(JavaCG2Constants.NEW_LINE).append(JavaCG2Constants.NEW_LINE);
        writer.write(data.toString());
    }

    /**
     * 生成标题序号
     *
     * @param level 标题级别
     * @return
     */
    private String genTitleSeq(int level) {
        if (level <= lastAddedTitleLevel) {
            // 当前插入的级别比上次的小或相同，需要将Map中当前插入级别之后的序号设为0
            for (int i = level + 1; ; i++) {
                if (titleLevelSeqMap.get(i) == null) {
                    break;
                }
                titleLevelSeqMap.put(i, 0);
            }
        }

        if (level == 1) {
            // 插入级别为1的标题
            lastAddedTitleLevel = level;
            return getTitleMaxSeq(level) + MarkdownConstants.FLAG_DOT;
        }

        // 插入级别大于1的标题
        if (level > lastAddedTitleLevel + 1) {
            throw new JavaCG2RuntimeException("本次添加的标题级别 " + level + " 不能超过 " + (lastAddedTitleLevel + 1));
        }

        // 处理从1到当前插入的最大级别之前的标题
        StringBuilder titleSeq = new StringBuilder();
        for (int currentLevel = 1; currentLevel < level; currentLevel++) {
            Integer currentSeq = titleLevelSeqMap.get(currentLevel);
            if (currentSeq == null) {
                currentSeq = 0;
            }
            titleSeq.append(currentSeq).append(MarkdownConstants.FLAG_DOT);
        }
        // 处理当前插入的最大级别的标题
        titleSeq.append(getTitleMaxSeq(level)).append(MarkdownConstants.FLAG_DOT);

        lastAddedTitleLevel = level;
        return titleSeq.toString();
    }

    /**
     * 获取需要添加的最大标题序号
     *
     * @param level
     * @return
     */
    private Integer getTitleMaxSeq(int level) {
        Integer levelSeq = titleLevelSeqMap.get(level);
        if (levelSeq == null) {
            levelSeq = 1;
        } else {
            levelSeq += 1;
        }

        titleLevelSeqMap.put(level, levelSeq);
        return levelSeq;
    }

    /**
     * 增加一行文本
     *
     * @param line 文本内容
     * @throws IOException
     */
    public void addLine(String line) throws IOException {
        writer.write(line + JavaCG2Constants.NEW_LINE);
    }

    /**
     * 增加一行空文本
     *
     * @throws IOException
     */
    public void addEmptyLine() throws IOException {
        writer.write(JavaCG2Constants.NEW_LINE);
    }

    /**
     * 增加一行文本，额外增加一个换行
     *
     * @param line 文本内容
     * @throws IOException
     */
    public void addLineWithNewLine(String line) throws IOException {
        writer.write(line + JavaCG2Constants.NEW_LINE + JavaCG2Constants.NEW_LINE);
    }

    /**
     * 添加代码块
     *
     * @param type 代码块类型
     * @throws IOException
     */
    public void addCodeBlock(MDCodeBlockTypeEnum type) throws IOException {
        codeBlockFlagNum++;
        if (codeBlockFlagNum % 2 == 0) {
            throw new JavaCG2RuntimeException(codeBlockFlagNum + "为偶数，代码块标志不能指定类型");
        }
        writer.write(MarkdownConstants.FLAG_CODE + type.getType() + JavaCG2Constants.NEW_LINE);
    }

    /**
     * 添加代码块
     *
     * @throws IOException
     */
    public void addCodeBlock() throws IOException {
        codeBlockFlagNum++;
        writer.write(MarkdownConstants.FLAG_CODE + JavaCG2Constants.NEW_LINE);
        if (codeBlockFlagNum % 2 == 0) {
            // 若为结束的代码块标志，需要额外加一行
            writer.write(JavaCG2Constants.NEW_LINE);
        }
    }

    /**
     * 添加列表
     *
     * @param data 列表内容
     * @throws IOException
     */
    public void addList(String data) throws IOException {
        writer.write(MarkdownConstants.FLAG_LIST + data + JavaCG2Constants.NEW_LINE);
    }

    /**
     * 添加列表，额外增加一个换行
     *
     * @param data 列表内容
     * @throws IOException
     */
    public void addListWithNewLine(String data) throws IOException {
        writer.write(MarkdownConstants.FLAG_LIST + data + JavaCG2Constants.NEW_LINE + JavaCG2Constants.NEW_LINE);
    }

    /**
     * 添加表格头
     *
     * @param columns
     */
    public void addTableHead(String... columns) throws IOException {
        if (ArrayUtils.isEmpty(columns)) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        tableColumnNum = columns.length;
        addLine(genTableContent(columns));

        StringBuilder tableLine = new StringBuilder(MarkdownConstants.FLAG_VERTICAL_BAR);
        for (int i = 0; i < tableColumnNum; i++) {
            tableLine.append(MarkdownConstants.FLAG_TABLE_LINE).append(MarkdownConstants.FLAG_VERTICAL_BAR);
        }
        addLine(tableLine.toString());
    }

    /**
     * 添加表格内容
     *
     * @param columns
     */
    public void addTableBody(String... columns) throws IOException {
        if (ArrayUtils.isEmpty(columns)) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        if (columns.length != tableColumnNum) {
            throw new JavaCG2RuntimeException("当前指定的列数量与表格头列数量不一致 " + columns.length + " " + tableColumnNum);
        }

        addLine(genTableContent(columns));
    }

    private String genTableContent(String... columns) {
        return MarkdownConstants.FLAG_VERTICAL_BAR + StringUtils.join(columns, MarkdownConstants.FLAG_VERTICAL_BAR) + MarkdownConstants.FLAG_VERTICAL_BAR;
    }

    @Override
    public void close() {
        if (writer != null) {
            try {
                writer.close();
            } catch (IOException e) {
                logger.error("error ", e);
            }
        }

        if (codeBlockFlagNum % 2 != 0) {
            throw new JavaCG2RuntimeException("代码块标志数量不是偶数 " + codeBlockFlagNum);
        }
    }

    //
    public boolean isWriteData() {
        return writeData;
    }

    public void setWriteData(boolean writeData) {
        this.writeData = writeData;
    }
}
