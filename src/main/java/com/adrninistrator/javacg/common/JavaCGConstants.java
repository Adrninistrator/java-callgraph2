package com.adrninistrator.javacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/26
 * @description:
 */

public class JavaCGConstants {
    /* JVM启动参数中的配置项 */
    // 保存配置文件的根目录
    public static final String PROPERTY_INPUT_ROOT_PATH = "input.root.path";

    // 调试日志打印开关值
    public static final String PROPERTY_VALUE_DEBUG_PRINT_IN_FILE = "file";

    // 文件列的分隔符
    public static final String FILE_COLUMN_SEPARATOR = "\t";

    // 输出文件中的常量
    public static final String FILE_KEY_JAR_INFO_PREFIX = "J";
    public static final String FILE_KEY_DIR_INFO_PREFIX = "D";
    public static final String FILE_KEY_CALL_TYPE_FLAG1 = "(";
    public static final String FILE_KEY_CALL_TYPE_FLAG2 = ")";
    public static final String FILE_KEY_METHOD_CALL_POSSIBLE_INFO_TYPE = "t";
    public static final String FILE_KEY_METHOD_CALL_POSSIBLE_INFO_VALUE = "v";
    public static final String FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE = "bv";
    public static final String FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD = "sf";
    public static final String FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD_METHOD_CALL = "sfm";
    public static final String FILE_KEY_EXTENDS = "e";
    public static final String FILE_KEY_IMPLEMENTS = "i";
    public static final String FILE_KEY_METHOD_ARGS_RETURN_TYPE = "t";
    public static final String FILE_KEY_METHOD_ARGS_RETURN_GENERICS_TYPE = "gt";

    public static final String FLAG_LAMBDA = "lambda$";
    public static final String FLAG_HASHTAG = "#";
    public static final String FLAG_ARRAY = "[]";
    public static final String FLAG_DOT = ".";
    public static final String FLAG_COLON = ":";
    public static final String FLAG_LEFT_BRACKET = "(";
    public static final String FLAG_RIGHT_BRACKET = ")";
    public static final String FLAG_COMMA = ",";
    public static final String FLAG_EQUAL = "=";

    public static final String EMPTY_METHOD_ARGS = FLAG_LEFT_BRACKET + FLAG_RIGHT_BRACKET;

    public static final int DEFAULT_LINE_NUMBER = 0;

    public static final String NEW_LINE = "\n";

    public static final String MERGED_JAR_FLAG = "-javacg_merged.jar";

    public static final String DIR_TAIL_OUTPUT = "-output_javacg";
    public static final String DIR_LOG = "log_javacg";
    public static final String DIR_CONFIG = "_javacg_config";

    public static final String FILE_PREFIX_LOG = "javacg-";

    public static final String FILE_CONFIG = "config.properties";

    public static final String EXT_JAR = ".jar";
    public static final String EXT_WAR = ".war";
    public static final String EXT_CLASS = ".class";
    public static final String EXT_TXT = ".txt";
    public static final String EXT_LOG = ".log";

    // 方法调用call_id起始值0（方法调用关系最小call_id为1）
    public static final int METHOD_CALL_ID_START = 0;

    // 处理父类和子类的方法调用时，节点索引的初始值
    public static final int EXTENDS_NODE_INDEX_INIT = -1;

    // 默认的jar包序号，使用0，因为实际的jar包序号从1开始
    public static final String DEFAULT_JAR_NUM = "0";

    // 将注解属性值写入文件时，\r替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN = 0x01;
    // 将注解属性值写入文件时，\n替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED = 0x02;

    // JSR指令类型
    public static final String JSR_TYPE = "0jsr";

    // 代表未使用的本地变量索引值
    public static final int LOCAL_VARIABLE_INDEX_NOT_USED = -1;

    // 方法调用可能的信息中，代表被调用对象的序号，使用0
    public static final int METHOD_CALL_POSSIBLE_INFO_SEQ_OBJECT = 0;

    // 方法调用可能的信息中，参数开始的序号，从1开始
    public static final int METHOD_CALL_POSSIBLE_INFO_SEQ_ARGS_START = 1;

    public static final int SIZE_10 = 10;
    public static final int SIZE_100 = 100;
    public static final int SIZE_200 = 200;
    public static final int SIZE_500 = 500;
    public static final int SIZE_1000 = 1000;

    public static final String[] FILE_EXT_ARRAY_XML = new String[]{"xml"};

    private JavaCGConstants() {
        throw new IllegalStateException("illegal");
    }
}
