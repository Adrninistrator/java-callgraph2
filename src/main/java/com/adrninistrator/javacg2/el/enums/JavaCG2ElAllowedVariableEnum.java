package com.adrninistrator.javacg2.el.enums;

import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/1/29
 * @description: 允许使用表达式的变量枚举
 */
public enum JavaCG2ElAllowedVariableEnum implements ElAllowedVariableInterface {

    // 合并jar/war文件、目录相关
    EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR("file_path", String.class.getSimpleName(), false,
            new String[]{"目录中的文件绝对路径", "以斜杠/为分隔符"},
            new String[]{"D:/a/b.jar", "/tmp/a/b.jar"}),
    EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR("file_dir_path", String.class.getSimpleName(), false,
            new String[]{"目录中的文件所在目录绝对路径", "以斜杠/为分隔符，不以分隔符结尾"},
            new String[]{"D:/a", "/tmp/a"}),
    EAVE_MF_FILE_PATH_IN_JAR_WAR("file_path", String.class.getSimpleName(), false,
            new String[]{"jar/war文件中的文件相对路径", "相对根目录的路径", "以斜杠/为分隔符，不以分隔符开头"},
            new String[]{"a/b/c.jar", "a/b/c.xml"}),
    EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR("file_dir_path", String.class.getSimpleName(), false,
            new String[]{"jar/war文件中的文件所在目录相对路径", "相对根目录的路径", "以斜杠/为分隔符，不以分隔符开头或结尾"},
            new String[]{"a/b", "a/b"}),
    EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR("class_file_path", String.class.getSimpleName(), false,
            new String[]{"jar/war文件中的class文件的相对路径", "相对根目录，或WEB-INF/classes、BOOT-INF/classes目录的路径", "以斜杠/为分隔符，不以分隔符开头"},
            new String[]{"a/b/c.class"}),
    EAVE_MF_FILE_NAME("file_name", String.class.getSimpleName(), false,
            new String[]{"文件名称"},
            new String[]{"a.class", "a.xml"}),
    EAVE_MF_OTHER_FILE_EXT("file_ext", String.class.getSimpleName(), false,
            new String[]{"非jar、war、class文件后缀", "以.开头"},
            new String[]{".xml", ".properties"}),
    EAVE_MF_CLASS_DIR_PREFIX_LEVEL("class_dir_prefix_level_", Set.class.getSimpleName(), true,
            new String[]{"jar/war文件中的class文件目录的不同层级的路径前缀集合"},
            new String[]{"集合：('a')", "集合：('a', 'a/b')"}),
    EAVE_PARSE_CLASS_NAME("class_name", String.class.getSimpleName(), false,
            new String[]{"完整类名"},
            new String[]{"a.b.Class1"}),
    EAVE_PARSE_PACKAGE_NAME("package_name", String.class.getSimpleName(), false,
            new String[]{"完整包名", "不会以.结束"},
            new String[]{"a.b"}),
    EAVE_PARSE_SIMPLE_CLASS_NAME("simple_class_name", String.class.getSimpleName(), false,
            new String[]{"简单类名"},
            new String[]{"Class1"}),
    EAVE_PARSE_METHOD_NAME("method_name", String.class.getSimpleName(), false,
            new String[]{"方法名称", "不包括括号及方法参数"},
            new String[]{"method1"}),
    EAVE_METHOD_CALL_TYPE("method_call_type", String.class.getSimpleName(), false,
            new String[]{"方法调用类型", "参考 JavaCG2CallTypeEnum 类"},
            new String[]{JavaCG2CallTypeEnum.getAllType4Show()}),
    EAVE_MC_ER_CLASS_NAME("er_class_name", String.class.getSimpleName(), false,
            new String[]{"调用方完整类名"},
            new String[]{"a.b.Class1"}),
    EAVE_MC_ER_PACKAGE_NAME("er_package_name", String.class.getSimpleName(), false,
            new String[]{"调用方完整包名", "不会以.结束"},
            new String[]{"a.b"}),
    EAVE_MC_ER_SIMPLE_CLASS_NAME("er_simple_class_name", String.class.getSimpleName(), false,
            new String[]{"调用方简单类名"},
            new String[]{"Class1"}),
    EAVE_MC_ER_METHOD_NAME("er_method_name", String.class.getSimpleName(), false,
            new String[]{"调用方方法名称", "不包括括号及方法参数"},
            new String[]{"method1"}),
    EAVE_MC_ER_METHOD_ARG_NUM("er_method_arg_num", int.class.getSimpleName(), false,
            new String[]{"调用方方法参数数量"},
            new String[]{"0", "1"}),
    EAVE_MC_ER_FULL_METHOD("er_full_method", String.class.getSimpleName(), false,
            new String[]{"调用方完整方法", "包括括号及方法参数"},
            new String[]{"a.b.Class1:method1(int)"}),
    EAVE_MC_EE_CLASS_NAME("ee_class_name", String.class.getSimpleName(), false,
            new String[]{"被调用方完整类名"},
            new String[]{"a.b.Class1"}),
    EAVE_MC_EE_PACKAGE_NAME("ee_package_name", String.class.getSimpleName(), false,
            new String[]{"被调用方完整包名", "不会以.结束"},
            new String[]{"a.b"}),
    EAVE_MC_EE_SIMPLE_CLASS_NAME("ee_simple_class_name", String.class.getSimpleName(), false,
            new String[]{"被调用方简单类名"},
            new String[]{"Class1"}),
    EAVE_MC_EE_METHOD_NAME("ee_method_name", String.class.getSimpleName(), false,
            new String[]{"被调用方方法名称", "不包括括号及方法参数"},
            new String[]{"method1"}),
    EAVE_MC_EE_METHOD_ARG_NUM("ee_method_arg_num", int.class.getSimpleName(), false,
            new String[]{"被调用方方法参数数量"},
            new String[]{"0", "1"}),
    EAVE_MC_EE_FULL_METHOD("ee_full_method", String.class.getSimpleName(), false,
            new String[]{"被调用方完整方法", "包括括号及方法参数"},
            new String[]{"a.b.Class1:method1(int)"}),
    ;

    // 变量名称
    private final String name;

    // 变量类型
    private final String type;

    // 是否为 ｛名称前缀}{数字} 的形式
    private final boolean prefixWithNum;

    // 变量说明
    private final String[] descriptions;

    // 变量值示例
    private final String[] valueExamples;

    JavaCG2ElAllowedVariableEnum(String name, String type, boolean prefixWithNum, String[] descriptions, String[] valueExamples) {
        this.name = name;
        this.type = type;
        this.prefixWithNum = prefixWithNum;
        this.descriptions = descriptions;
        this.valueExamples = valueExamples;
    }

    @Override
    public String getVariableName() {
        return name;
    }

    @Override
    public String getType() {
        return type;
    }

    @Override
    public boolean isPrefixWithNum() {
        return prefixWithNum;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String[] getValueExamples() {
        return valueExamples;
    }
}
