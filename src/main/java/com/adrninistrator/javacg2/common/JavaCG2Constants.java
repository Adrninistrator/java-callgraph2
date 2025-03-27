package com.adrninistrator.javacg2.common;


/**
 * @author adrninistrator
 * @date 2021/6/26
 * @description:
 */

public class JavaCG2Constants {
    /* JVM启动参数中的配置项 */
    // 保存配置文件的根目录
    public static final String PROPERTY_INPUT_ROOT_PATH = "input.root.path";

    // 输出文件中的常量
    // jar/war文件
    public static final String FILE_KEY_JAR = "J";
    // 目录
    public static final String FILE_KEY_DIR = "D";
    // jar/war文件中的jar文件
    public static final String FILE_KEY_JAR_IN_JAR = "JIJ";

    // 解析结果文件保存目录
    public static final String FILE_KEY_RESULT_DIR_INFO_PREFIX = "R";
    // 方法调用类型左边的标记
    public static final String FILE_KEY_CALL_TYPE_FLAG1 = "(";
    // 方法调用类型右边的标记
    public static final String FILE_KEY_CALL_TYPE_FLAG2 = ")";
    // 继承
    public static final String FILE_KEY_EXTENDS = "e";
    // 实现
    public static final String FILE_KEY_IMPLEMENTS = "i";
    // 类型
    public static final String FILE_KEY_CLASS_TYPE = "t";
    // 泛型类型
    public static final String FILE_KEY_GENERICS_TYPE = "gt";
    // JDK中的类型
    public static final String FILE_KEY_CATEGORY_JDK = "J";
    // 自定义类型
    public static final String FILE_KEY_CATEGORY_CUSTOM = "C";
    // 泛型类型，只涉及JDK中的类型
    public static final String FILE_KEY_CATEGORY_GENERICS_JDK = "GJ";
    // 泛型类型，涉及自定义类型
    public static final String FILE_KEY_CATEGORY_GENERICS_CUSTOM = "GC";
    // 方法中通过catch捕获的异常标志，编译器为switch生成的catch代码块
    public static final String FILE_KEY_CATCH_FLAG_SWITCH = "switch";
    // 方法中通过catch捕获的异常标志，编译器为try-with-resource生成的catch代码块
    public static final String FILE_KEY_CATCH_FLAG_TRY_WITH_RESOURCE = "try-with-resource";
    // 方法中通过throw抛出的异常类型，catch的异常对象
    public static final String FILE_KEY_THROW_TYPE_CATCH_EXCEPTION = "ce";
    // 方法中通过throw抛出的异常类型，方法调用返回值
    public static final String FILE_KEY_THROW_TYPE_METHOD_CALL_RETURN = "mcr";
    // 方法中通过throw抛出的异常类型，未知情况
    public static final String FILE_KEY_THROW_TYPE_UNKNOWN = "unk";
    // Spring Bean的定义方式，Java代码中定义
    public static final String FILE_KEY_SPRING_BEAN_IN_JAVA = "j";
    // Spring Bean的定义方式，XML文件中定义
    public static final String FILE_KEY_SPRING_BEAN_IN_XML = "x";

    // 配置类型
    public static final String CONFIG_PROPERTIES = "properties";
    public static final String CONFIG_LIST = "list";
    public static final String CONFIG_SET = "set";

    public static final String FLAG_HASHTAG = "#";
    public static final String FLAG_ARRAY = "[]";
    public static final String FLAG_DOT = ".";
    public static final String FLAG_COLON = ":";
    public static final String FLAG_LEFT_BRACKET = "(";
    public static final String FLAG_RIGHT_BRACKET = ")";
    public static final String FLAG_COMMA = ",";
    public static final String FLAG_EQUAL = "=";
    public static final String FLAG_DOLOR = "$";
    public static final String FLAG_TAB = "\t";
    public static final String FLAG_SLASH = "/";

    public static final String COMMENT_PROPERTIES = "# ";
    public static final String COMMENT_AVIATOR = "## ";

    public static final String EMPTY_METHOD_ARGS = FLAG_LEFT_BRACKET + FLAG_RIGHT_BRACKET;

    public static final int DEFAULT_LINE_NUMBER = 0;
    public static final String DEFAULT_LINE_NUMBER_STR = String.valueOf(DEFAULT_LINE_NUMBER);

    public static final String NEW_LINE = "\n";
    public static final String NEW_LINE_WINDOWS = "\r\n";

    public static final String JAR_SEQ_FLAG = "@";
    public static final String MERGED_JAR_FLAG = "-javacg2_merged.jar";

    public static final String DIR_TAIL_OUTPUT = "-output_javacg2";

    public static final String EL_IGNORE_DATA_FILE_NAME = "el_ignore_data.log";

    // 保存全部的配置参数信息文件
    public static final String FILE_JAVACG2_ALL_CONFIG_MD = "_javacg2_all_config.md";
    // 保存当前有使用的配置参数信息文件
    public static final String FILE_JAVACG2_USED_CONFIG_MD = "_javacg2_used_config.md";

    public static final String FILE_TYPE_CLASS = "class";

    public static final String EXT_JAR = ".jar";
    public static final String EXT_WAR = ".war";
    public static final String EXT_CLASS = "." + FILE_TYPE_CLASS;
    public static final String EXT_TXT = ".txt";
    public static final String EXT_MD = ".md";

    public static final String WEB_INF_CLASSES = "WEB-INF/classes/";
    public static final String BOOT_INF_CLASSES = "BOOT-INF/classes/";

    public static final String THIS = "this";

    // 方法调用call_id最小值1
    public static final int METHOD_CALL_ID_MIN = 1;
    // 方法调用call_id最小值再减1
    public static final int METHOD_CALL_ID_MIN_BEFORE = METHOD_CALL_ID_MIN - 1;

    // 记录id，最小值
    public static final int RECORD_ID_MIN = 1;
    // 记录id，最小值再减1
    public static final int RECORD_ID_MIN_BEFORE = RECORD_ID_MIN - 1;

    // jar包序号最小值
    public static final int JAR_NUM_MIN = 1;
    public static final int JAR_NUM_MIN_BEFORE = JAR_NUM_MIN - 1;

    // 代表为空的jar包序号
    public static final String EMPTY_JAR_NUM = "-";

    // 将注解属性值写入文件时，\r替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN = 0x01;
    // 将注解属性值写入文件时，\n替换后的字符
    public static final char ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED = 0x02;

    // JSR指令类型
    public static final String JSR_TYPE = "0jsr";

    // 代表未使用的本地变量索引值
    public static final int LOCAL_VARIABLE_INDEX_NOT_USED = -1;

    // 方法调用中，代表被调用对象的序号，使用0
    public static final int METHOD_CALL_OBJECT_SEQ = 0;

    // 方法调用中，参数开始的序号，从1开始
    public static final int METHOD_CALL_ARGUMENTS_START_SEQ = 1;

    public static final int SIZE_10 = 10;
    public static final int SIZE_100 = 100;
    public static final int SIZE_200 = 200;
    public static final int SIZE_500 = 500;
    public static final int SIZE_1000 = 1000;

    public static final String METHOD_PREFIX_GET = "get";
    public static final String METHOD_PREFIX_IS = "is";
    public static final String METHOD_PREFIX_SET = "set";

    public static final String TRY = "try";
    public static final String CATCH = "catch";

    public static final int MAX_FRAME_SNAP_SHOTS_NUM = 50000;

    // 表达式用于写忽略数据文件的线程名称前缀
    public static final String THREAD_NAME_PREFIX_EL_WRITE_IGNORE_DATA = "el_write_ignore_data";

    // 等值转换方法
    public static final String[] FR_EQ_CONVERSION_METHODS = new String[]{
            "java.lang.Boolean:<init>=1",
            "java.lang.Boolean:valueOf=1",
            "java.lang.Boolean:parseBoolean=1",
            "java.lang.Integer:<init>=1",
            "java.lang.Integer:valueOf=1",
            "java.lang.Integer:parseInt=1",
            "java.lang.Long:<init>=1",
            "java.lang.Long:valueOf=1",
            "java.lang.Long:parseLong=1",
            "java.lang.Float:<init>=1",
            "java.lang.Float:valueOf=1",
            "java.lang.Float:parseFloat=1",
            "java.lang.Double:<init>=1",
            "java.lang.Double:valueOf=1",
            "java.lang.Double:parseDouble=1",
            "java.lang.String:<init>=1",
            "java.lang.String:valueOf=1",
            "java.lang.String:trim=0",
            "java.math.BigDecimal:<init>=1",
            "java.math.BigDecimal:valueOf=1",
            "java.math.BigDecimal:toString=0",
            "java.math.BigDecimal:=0",
            "org.apache.commons.lang3.StringUtils:defaultIfBlank=1",
            "org.apache.commons.lang3.StringUtils:defaultIfEmpty=1",
            "org.apache.commons.lang3.StringUtils:defaultString=1",
            "org.apache.commons.lang3.StringUtils:trim=1",
            "org.apache.commons.lang.StringUtils:defaultIfBlank=1",
            "org.apache.commons.lang.StringUtils:defaultIfEmpty=1",
            "org.apache.commons.lang.StringUtils:defaultString=1",
            "org.apache.commons.lang.StringUtils:trim=1",
            "org.apache.commons.lang3.math.NumberUtils:createBigDecimal=1",
            "org.apache.commons.lang3.math.NumberUtils:createBigInteger=1",
            "org.apache.commons.lang3.math.NumberUtils:createDouble=1",
            "org.apache.commons.lang3.math.NumberUtils:createFloat=1",
            "org.apache.commons.lang3.math.NumberUtils:createInteger=1",
            "org.apache.commons.lang3.math.NumberUtils:createLong=1",
            "org.apache.commons.lang3.math.NumberUtils:createNumber=1",
            "org.apache.commons.lang.math.NumberUtils:createBigDecimal=1",
            "org.apache.commons.lang.math.NumberUtils:createBigInteger=1",
            "org.apache.commons.lang.math.NumberUtils:createDouble=1",
            "org.apache.commons.lang.math.NumberUtils:createFloat=1",
            "org.apache.commons.lang.math.NumberUtils:createInteger=1",
            "org.apache.commons.lang.math.NumberUtils:createLong=1",
            "org.apache.commons.lang.math.NumberUtils:createNumber=1"
    };

    private JavaCG2Constants() {
        throw new IllegalStateException("illegal");
    }
}
