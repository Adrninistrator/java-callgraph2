package com.adrninistrator.javacg2.common.enums;

/**
 * @author adrninistrator
 * @date 2025/1/30
 * @description:
 */
public enum JavaCG2DirEnum {

    IDE_CONFIG("_javacg2_config", "java-callgraph2组件主要配置文件所在目录", true),
    IDE_MERGE_FILE_SWITCH("_javacg2_merge_file_switch", "合并jar/war文件、目录相关开关配置文件目录", true),
    IDE_PARSE_CLASS_METHOD_SWITCH("_javacg2_parse_class_method_switch", "解析类、方法相关开关配置文件所在目录", true),
    IDE_METHOD_CALL_SWITCH("_javacg2_parse_method_call_switch", "解析方法调用相关开关配置文件所在目录", true),
    IDE_EL_EXAMPLE("_el_example", "el表达式示例配置文件所在目录", true),
    IDE_FAIL_CLASSES("javacg2_fail_classes", "解析失败的class文件所在目录", false),
    ;

    private final String dirName;
    private final String description;
    private final boolean input;

    JavaCG2DirEnum(String dirName, String description, boolean input) {
        this.dirName = dirName;
        this.description = description;
        this.input = input;
    }

    public String getDirName() {
        return dirName;
    }

    public String getDescription() {
        return description;
    }

    public boolean isInput() {
        return input;
    }

    @Override
    public String toString() {
        return dirName;
    }
}
