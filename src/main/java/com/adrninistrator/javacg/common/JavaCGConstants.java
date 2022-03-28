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
    public static final String FLAG_VERTICAL_BAR = "\\|";

    public static final int DEFAULT_LINE_NUMBER = 0;
    public static final int NONE_LINE_NUMBER = -1;

    public static final String METHOD_NAME_INIT = "<init>";
    public static final String METHOD_NAME_START = "start";

    public static final String NEW_LINE = "\n";

    public static final String OBJECT_CLASS_NAME = Object.class.getName();

    public static final String MERGED_JAR_FLAG = "-javacg_merged.jar";

    public static final String FILE_FLAG_ANNOTATION = "-annotation";

    public static final String EXT_JAR = ".jar";
    public static final String EXT_WAR = ".war";
    public static final String EXT_CLASS = ".class";
    public static final String EXT_TXT = ".txt";

    // 默认的jar包序号，使用0，因为实际的jar包序号从1开始
    public static final int DEFAULT_JAR_NUM = 0;

    private JavaCGConstants() {
        throw new IllegalStateException("illegal");
    }
}
