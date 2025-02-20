package com.adrninistrator.javacg2.el.dto;

/**
 * @author adrninistrator
 * @date 2025/1/27
 * @description: 表达式变量配置
 */
public class ElVariableConfig {

    // 变量名称
    private String name;

    // 变量类型
    private String type;

    // 变量说明
    private String[] descriptions;

    // 变量值示例
    private String[] valueExamples;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String[] getDescriptions() {
        return descriptions;
    }

    public void setDescriptions(String[] descriptions) {
        this.descriptions = descriptions;
    }

    public String[] getValueExamples() {
        return valueExamples;
    }

    public void setValueExamples(String[] valueExamples) {
        this.valueExamples = valueExamples;
    }
}
