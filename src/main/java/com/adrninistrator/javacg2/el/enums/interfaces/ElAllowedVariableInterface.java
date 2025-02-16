package com.adrninistrator.javacg2.el.enums.interfaces;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 表达式语言允许使用的变量接口
 */
public interface ElAllowedVariableInterface {

    // 获取变量名称
    String getVariableName();

    // 获取变量类型
    String getType();

    // 获取变量描述
    String[] getDescriptions();

    // 获取变量示例
    String[] getValueExamples();
}
