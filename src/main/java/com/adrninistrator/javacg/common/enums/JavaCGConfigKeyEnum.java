package com.adrninistrator.javacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCGConfigKeyEnum {
    CKE_PARSE_METHOD_CALL_TYPE_VALUE("parse.method.call.type.value", "处理方法调用时是否解析被调用对象和参数可能的类型与值"),
    CKE_FIRST_PARSE_INIT_METHOD_TYPE("first.parse.init.method.type", "处理类的方法前是否需要先解析构造函数以获取非静态字段可能的类型，仅当parse.method.call.type.value参数为true时才可以生效"),
    CKE_ANALYSE_FIELD_RELATIONSHIP("analyse.field.relationship", "是否需要分析dto的字段之间的关联关系，仅当parse.method.call.type.value参数为true时才可以生效"),
    CKE_CONTINUE_WHEN_ERROR("continue.when.error", "解析方法出现异常时，是否要继续。true: 继续；false: 不继续"),
    CKE_LOG_METHOD_SPEND_TIME("log.method.spend.time", "记录方法分析耗时的开关（开启后会在输出目录中生成相关文件）。true: 开启；false: 关闭"),
    CKE_OUTPUT_ROOT_PATH("output.root.path", "生成文件的根目录，以\"/\"或\"\\\\\"作为分隔符，末尾是否为分隔符不影响（默认为jar包所在目录）"),
    CKE_OUTPUT_FILE_EXT("output.file.ext", "生成文件后缀名（默认为.txt）"),
    ;

    private final String key;
    private final String desc;

    JavaCGConfigKeyEnum(String key, String desc) {
        this.key = key;
        this.desc = desc;
    }

    public String getKey() {
        return key;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return key;
    }
}
