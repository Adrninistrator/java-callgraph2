package com.adrninistrator.javacg2.el.enums;

import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2025/8/20
 * @description: 公共的允许使用表达式的变量枚举
 */
public enum CommonElAllowedVariableEnum implements ElAllowedVariableInterface {

    EAVE_METHOD_CALL_TYPE("method_call_type", String.class.getSimpleName(),
            new String[]{"方法调用类型", "参考 " + JavaCG2CallTypeEnum.class.getSimpleName() + " 类"},
            new String[]{JavaCG2CallTypeEnum.getAllType4Show()}),
    EAVE_MC_ER_CLASS_NAME("er_class_name", String.class.getSimpleName(),
            new String[]{"调用方完整类名"},
            new String[]{"a.b.Class1"}),
    EAVE_MC_ER_PACKAGE_NAME("er_package_name", String.class.getSimpleName(),
            new String[]{"调用方完整包名", "不会以.结束"},
            new String[]{"a.b"}),
    EAVE_MC_ER_SIMPLE_CLASS_NAME("er_simple_class_name", String.class.getSimpleName(),
            new String[]{"调用方简单类名"},
            new String[]{"Class1"}),
    EAVE_MC_ER_METHOD_NAME("er_method_name", String.class.getSimpleName(),
            new String[]{"调用方方法名", "不包括括号及方法参数"},
            new String[]{"method1"}),
    EAVE_MC_ER_METHOD_ARG_NUM("er_method_arg_num", int.class.getSimpleName(),
            new String[]{"调用方方法参数数量"},
            new String[]{"0", "1"}),
    EAVE_MC_ER_FULL_METHOD("er_full_method", String.class.getSimpleName(),
            new String[]{"调用方完整方法", "包括括号及方法参数"},
            new String[]{"a.b.Class1:method1(int)"}),
    EAVE_MC_EE_CLASS_NAME("ee_class_name", String.class.getSimpleName(),
            new String[]{"被调用方完整类名"},
            new String[]{"a.b.Class1"}),
    EAVE_MC_EE_PACKAGE_NAME("ee_package_name", String.class.getSimpleName(),
            new String[]{"被调用方完整包名", "不会以.结束"},
            new String[]{"a.b"}),
    EAVE_MC_EE_SIMPLE_CLASS_NAME("ee_simple_class_name", String.class.getSimpleName(),
            new String[]{"被调用方简单类名"},
            new String[]{"Class1"}),
    EAVE_MC_EE_METHOD_NAME("ee_method_name", String.class.getSimpleName(),
            new String[]{"被调用方方法名", "不包括括号及方法参数"},
            new String[]{"method1"}),
    EAVE_MC_EE_METHOD_ARG_NUM("ee_method_arg_num", int.class.getSimpleName(),
            new String[]{"被调用方方法参数数量"},
            new String[]{"0", "1"}),
    EAVE_MC_EE_FULL_METHOD("ee_full_method", String.class.getSimpleName(),
            new String[]{"被调用方完整方法", "包括括号及方法参数"},
            new String[]{"a.b.Class1:method1(int)"}),
    ;

    // 变量名称
    private final String name;

    // 变量类型
    private final String type;

    // 变量说明
    private final String[] descriptions;

    // 变量值示例
    private final String[] valueExamples;

    CommonElAllowedVariableEnum(String name, String type, String[] descriptions, String[] valueExamples) {
        this.name = name;
        this.type = type;
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
        return false;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String[] getValueExamples() {
        return valueExamples;
    }

    @Override
    public String getEnumConstantName() {
        return name();
    }

    @Override
    public String toString() {
        throw new JavaCG2RuntimeException("为避免误用，当前方法不允许调用，应当使用 getVariableName 方法");
    }
}