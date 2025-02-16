package com.adrninistrator.javacg2.markdown;

/**
 * @author adrninistrator
 * @date 2023/2/17
 * @description: markdown相关常量
 */
public class MarkdownConstants {

    public static final String FLAG_TITLE = "#";
    public static final String FLAG_SPACE = " ";
    public static final String FLAG_CODE = "```";
    public static final String FLAG_LIST = "- ";
    public static final String FLAG_DOT = ".";
    public static final String FLAG_VERTICAL_BAR = "|";
    public static final String FLAG_TABLE_LINE = "---";
    public static final String FLAG_HTML_NEW_LINE = "<br>";

    private MarkdownConstants() {
        throw new IllegalStateException("illegal");
    }
}
