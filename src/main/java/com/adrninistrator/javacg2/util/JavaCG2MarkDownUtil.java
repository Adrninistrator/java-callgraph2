package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.markdown.MarkdownConstants;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2025/11/19
 * @description:
 */
public class JavaCG2MarkDownUtil {

    /**
     * 添加表格头
     *
     * @param columns
     */
    public static String addTableHead(String... columns) {
        if (ArrayUtils.isEmpty(columns)) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(addLine(genTableContent(columns)));

        stringBuilder.append(MarkdownConstants.FLAG_VERTICAL_BAR);
        for (int i = 0; i < columns.length; i++) {
            stringBuilder.append(MarkdownConstants.FLAG_TABLE_LINE).append(MarkdownConstants.FLAG_VERTICAL_BAR);
        }
        return addLine(stringBuilder.toString());
    }

    /**
     * 添加表格内容
     *
     * @param columns
     */
    public static String addTableBody(String... columns) {
        if (ArrayUtils.isEmpty(columns)) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        return addLine(genTableContent(columns));
    }

    /**
     * 生成表格内容
     *
     * @param columns
     * @return
     */
    public static String genTableContent(String... columns) {
        return MarkdownConstants.FLAG_VERTICAL_BAR + StringUtils.join(columns, MarkdownConstants.FLAG_VERTICAL_BAR) + MarkdownConstants.FLAG_VERTICAL_BAR;
    }

    /**
     * 增加一行文本
     *
     * @param line 文本内容
     * @
     */
    public static String addLine(String line) {
        return line + JavaCG2Constants.NEW_LINE;
    }

    private JavaCG2MarkDownUtil() {
        throw new IllegalStateException("illegal");
    }
}
