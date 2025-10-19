package com.adrninistrator.javacg2.el.enums.interfaces;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 表达式允许使用的变量接口
 */
public interface ElAllowedVariableInterface {

    // 获取变量名称
    String getVariableName();

    // 获取变量类型
    String getType();

    // 是否为 ｛名称前缀}{数字} 的形式
    boolean isPrefixWithNum();

    // 获取变量描述
    String[] getDescriptions();

    // 获取变量示例
    String[] getValueExamples();

    // 获取枚举常量名称
    String getEnumConstantName();
}
