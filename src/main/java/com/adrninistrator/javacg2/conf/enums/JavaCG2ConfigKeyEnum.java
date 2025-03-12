package com.adrninistrator.javacg2.conf.enums;

import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCG2ConfigKeyEnum implements MainConfigInterface {
    CKE_PARSE_METHOD_CALL_TYPE_VALUE("parse.method.call.type.value",
            new String[]{"处理方法调用时是否解析被调用对象和参数可能的类型与值"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_FIRST_PARSE_INIT_METHOD_TYPE("first.parse.init.method.type",
            new String[]{"处理类的方法前是否需要先解析构造函数以获取非静态字段可能的类型，仅当parse.method.call.type.value参数为true时才可以生效"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_ANALYSE_FIELD_RELATIONSHIP("analyse.field.relationship",
            new String[]{"是否需要分析dto的字段之间的关联关系，仅当parse.method.call.type.value参数为true时才可以生效"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_CONTINUE_WHEN_ERROR("continue.when.error",
            new String[]{"解析方法出现异常时，是否要继续。true: 继续；false: 不继续"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_LOG_METHOD_SPEND_TIME("log.method.spend.time",
            new String[]{"记录方法分析耗时的开关（开启后会在输出目录中生成相关文件）。true: 开启；false: 关闭"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_OUTPUT_ROOT_PATH("output.root.path",
            new String[]{"生成文件的根目录，以\"/\"或\"\\\\\"作为分隔符，末尾是否为分隔符不影响（默认为jar包所在目录）"},
            String.class, false, ""),
    CKE_OUTPUT_FILE_EXT("output.file.ext",
            new String[]{"生成文件后缀名"},
            String.class, false, ".txt"),
    CKE_EL_DEBUG_MODE("el.debug.mode",
            new String[]{"表达式执行时是否开启调试模式，若开启会在应用日志中输出表达式执行时的详细信息"},
            Boolean.class, false, Boolean.FALSE.toString()),
    ;

    // 参数key
    private final String key;
    // 参数描述
    private final String[] descriptions;
    // 参数类型
    private final Class<?> type;
    // 是否不允许为空
    private final boolean notBlank;
    // 默认值
    private final String defaultValue;

    JavaCG2ConfigKeyEnum(String key, String[] descriptions, Class<?> type, boolean notBlank, String defaultValue) {
        this.key = key;
        this.descriptions = descriptions;
        this.type = type;
        this.notBlank = notBlank;
        this.defaultValue = defaultValue;
    }

    @Override
    public String getEnumName() {
        return name();
    }

    @Override
    public String getKey() {
        return key;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String getConfigPrintInfo() {
        return key + " " + JavaCG2ConfigKeyEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    @Override
    public boolean notBlank() {
        return notBlank;
    }

    @Override
    public String getDefaultValue() {
        return defaultValue;
    }

    @Override
    public String toString() {
        return key;
    }

    @Override
    public String getFileName() {
        return JavaCG2DirEnum.IDE_CONFIG.getDirName() + "/config.properties";
    }
}
