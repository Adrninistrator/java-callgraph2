package com.adrninistrator.javacg2.el.handler;

import com.adrninistrator.javacg2.el.dto.ElVariableConfig;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.el.function.StringContainsAnyFunction;
import com.adrninistrator.javacg2.el.function.StringEndsWithAnyFunction;
import com.adrninistrator.javacg2.el.function.StringEqualsAnyFunction;
import com.adrninistrator.javacg2.el.function.StringStartsWithAnyFunction;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.utils.ArrayHashMap;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/1/27
 * @description: 表达式语言处理类
 */
public class ElHandler {

    private static final Logger logger = LoggerFactory.getLogger(ElHandler.class);

    // 表达式文本
    private final String expressionText;

    // 表达式语言允许的变量配置列表
    private final List<ElVariableConfig> elAllowedVariableConfigList = new ArrayList<>();

    // 表达式语言允许的变量名称Set
    private final Set<String> elAllowedVariableNameSet = new HashSet<>();

    // 表达式语言有指定的变量名称Set
    private final Set<String> elSpecifiedVariableNameSet = new HashSet<>();

    // 表达式
    private Expression expression;

    /**
     * @param expressionText         表达式文本
     * @param elAllowedVariableEnums 允许的表达式语言变量枚举
     */
    public ElHandler(String expressionText, ElAllowedVariableInterface[] elAllowedVariableEnums) {
        this.expressionText = expressionText;
        if (StringUtils.isBlank(expressionText)) {
            // 若表达式语言文本为空，则不创建 Expression 对象
            return;
        }

        if (ArrayUtils.isEmpty(elAllowedVariableEnums)) {
            throw new JavaCG2RuntimeException("允许的表达式语言变量信息不能为空");
        }

        // 检查指定的变量名称是否允许使用
        for (ElAllowedVariableInterface elAllowedVariableEnum : elAllowedVariableEnums) {
            addElVariableConfig(elAllowedVariableEnum);
            elSpecifiedVariableNameSet.add(elAllowedVariableEnum.getVariableName());
        }

        AviatorEvaluatorInstance aviatorEvaluatorInstance = AviatorEvaluator.newInstance();
        aviatorEvaluatorInstance.enableSandboxMode();
        aviatorEvaluatorInstance.addFunction(new StringContainsAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringEndsWithAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringEqualsAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringStartsWithAnyFunction());

        expression = aviatorEvaluatorInstance.compile(expressionText, true);
        for (String variableName : expression.getVariableNames()) {
            if (!elAllowedVariableNameSet.contains(variableName)) {
                StringBuilder stringBuilder = new StringBuilder();
                for (ElVariableConfig elVariableConfig : elAllowedVariableConfigList) {
                    stringBuilder.append("\n")
                            .append(elVariableConfig.getName()).append("\t")
                            .append(elVariableConfig.getType()).append("\t")
                            .append(StringUtils.join(elVariableConfig.getDescriptions(), " ")).append("\t")
                            .append(StringUtils.join(elVariableConfig.getValueExamples(), " "));
                }
                logger.error("{} 当前使用的表达式变量名称非法，允许使用的变量如下\n" +
                        "变量名称\t变量类型\t变量说明\t变量值示例{}", this.getClass().getSimpleName(), stringBuilder);
                throw new JavaCG2RuntimeException("当前使用的表达式变量名称非法");
            }
        }
    }

    /**
     * 增加表达式语言变量配置
     *
     * @param elAllowedVariableEnum 允许使用表达式语言的变量枚举
     */
    protected void addElVariableConfig(ElAllowedVariableInterface elAllowedVariableEnum) {
        if (!elAllowedVariableNameSet.add(elAllowedVariableEnum.getVariableName())) {
            throw new JavaCG2RuntimeException("添加了重复的变量名称 " + elAllowedVariableEnum.getVariableName());
        }
        ElVariableConfig elVariableConfig = new ElVariableConfig();
        elVariableConfig.setName(elAllowedVariableEnum.getVariableName());
        elVariableConfig.setType(elAllowedVariableEnum.getType());
        elVariableConfig.setDescriptions(elAllowedVariableEnum.getDescriptions());
        elVariableConfig.setValueExamples(elAllowedVariableEnum.getValueExamples());
        elAllowedVariableConfigList.add(elVariableConfig);
    }

    /**
     * 检查是否有在表达式语言中指定变量名称
     *
     * @param elVariable 变量枚举
     * @return
     */
    public boolean checkVariableNameSpecified(ElAllowedVariableInterface elVariable) {
        return elSpecifiedVariableNameSet.contains(elVariable.getVariableName());
    }

    /**
     * 执行表达式
     *
     * @param map 包含变量名称与值的Map
     * @return
     */
    public boolean runExpression(Map<String, Object> map) {
        if (expression == null) {
            // 若 Expression 对象为空，说明未指定表达式文本，则默认返回false
            return false;
        }

        Object result = expression.execute(map);
        if (!(result instanceof Boolean)) {
            logger.error("{} 表达式执行结果返回值非 {} [{}]", expressionText, Boolean.class.getSimpleName(), expressionText);
            throw new JavaCG2RuntimeException("表达式执行结果返回值非法");
        }
        return (boolean) result;
    }

    /**
     * 生成表达式语言用于执行的Map
     *
     * @return null: 未指定表达式文本 非null: 有指定表达式文本
     */
    public Map<String, Object> genMap4ElExecute() {
        if (expression == null) {
            // 若未指定表达式文本，则默认返回null
            return null;
        }
        return new ArrayHashMap<>();
    }
}
