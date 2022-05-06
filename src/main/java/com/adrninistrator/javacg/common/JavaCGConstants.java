package com.adrninistrator.javacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/26
 * @description:
 */

public class JavaCGConstants {

    // 记录所有的接口调用实现类，及子类调用父类方法
    public static final String PROPERTY_RECORD_ALL = "record.all";
    // 调试日志打印
    public static final String PROPERTY_DEBUG_PRINT = "debug.print";
    // 合并jar/war包中的class文件时，需要合并的包名
    public static final String PROPERTY_MERGE_CLASS_IN_JAR_PACKAGE = "merge.class.in.jar.package";

    public static final String FILE_KEY_CLASS_PREFIX = "C:";
    public static final String FILE_KEY_METHOD_PREFIX = "M:";
    public static final String FILE_KEY_JAR_INFO_PREFIX = "J:";
    public static final String FILE_KEY_DIR_INFO_PREFIX = "D:";

    public static final String FLAG_LAMBDA = "lambda$";
    public static final String FLAG_HASHTAG = "#";
    public static final String FLAG_ARRAY = "[]";

    public static final int DEFAULT_LINE_NUMBER = 0;
    public static final int NONE_LINE_NUMBER = -1;

    public static final String METHOD_NAME_INIT = "<init>";
    public static final String METHOD_NAME_START = "start";

    public static final String NEW_LINE = "\n";

    public static final String OBJECT_CLASS_NAME = Object.class.getName();

    public static final String MERGED_JAR_FLAG = "-javacg_merged.jar";

    public static final String FILE_FLAG_ANNOTATION = "-annotation";
    public static final String FILE_FLAG_LINE_NUMBER = "-line_number";

    public static final String EXT_JAR = ".jar";
    public static final String EXT_WAR = ".war";
    public static final String EXT_CLASS = ".class";
    public static final String EXT_TXT = ".txt";

    // 默认的jar包序号，使用0，因为实际的jar包序号从1开始
    public static final int DEFAULT_JAR_NUM = 0;

    // 输出的注解信息文件，包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE = 5;
    // 输出的注解信息文件，不包含属性时的列数
    public static final int ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE = 3;

    // 将注解属性值写入文件时，空格替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_BACKSPACE = 0x01;
    // 将注解属性值写入文件时，\r替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN = 0x02;
    // 将注解属性值写入文件时，\n替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED = 0x03;

    // 输出的方法行号信息文件的列数
    public static final int LINE_NUMBER_COLUMN_NUM = 3;

    private JavaCGConstants() {
        throw new IllegalStateException("illegal");
    }
}
