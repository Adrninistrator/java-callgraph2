package com.adrninistrator.javacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/26
 * @description:
 */

public class JavaCGConstants {

    public static final String KEY_RECORD_ALL = "record.all";
    public static final String KEY_DEBUG_PRINT = "debug.print";

    public static final String FILE_KEY_CLASS_PREFIX = "C:";
    public static final String FILE_KEY_METHOD_PREFIX = "M:";
    public static final String FILE_KEY_JAR_INFO_PREFIX = "J:";
    public static final String FILE_KEY_DIR_INFO_PREFIX = "D:";

    public static final String FLAG_LAMBDA = "lambda$";

    public static final int DEFAULT_LINE_NUMBER = 0;
    public static final int NONE_LINE_NUMBER = -1;

    public static final String METHOD_NAME_INIT = "<init>";

    public static final String METHOD_NAME_START = "start";

    public static final String NEW_LINE = "\n";

    public static final String OBJECT_CLASS_NAME = Object.class.getName();

    public static final String MERGED_JAR_FLAG = "-javacg_merged.jar";

    public static final String FILE_FLAG_ANNOTATION = "-annotation";

    public static final String EXT_JAR = ".jar";
    public static final String EXT_TXT = ".txt";

    // 默认的jar包序号，使用0，因为实际的jar包序号从1开始
    public static final int DEFAULT_JAR_NUM = 0;

    public static final String ONE_JAR_INFO_KEY = "key";

    private JavaCGConstants() {
        throw new IllegalStateException("illegal");
    }
}
