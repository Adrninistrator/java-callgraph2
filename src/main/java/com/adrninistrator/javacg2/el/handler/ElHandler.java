package com.adrninistrator.javacg2.el.handler;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.el.dto.ElVariableConfig;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.function.any.StringContainsAnyFunction;
import com.adrninistrator.javacg2.el.function.any.StringEndsWithAnyFunction;
import com.adrninistrator.javacg2.el.function.any.StringEqualsAnyFunction;
import com.adrninistrator.javacg2.el.function.any.StringStartsWithAnyFunction;
import com.adrninistrator.javacg2.el.function.ignorecase.StringContainsICFunction;
import com.adrninistrator.javacg2.el.function.ignorecase.StringEndsWithICFunction;
import com.adrninistrator.javacg2.el.function.ignorecase.StringEqualsICFunction;
import com.adrninistrator.javacg2.el.function.ignorecase.StringStartsWithICFunction;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.utils.ArrayHashMap;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @author adrninistrator
 * @date 2025/1/27
 * @description: 表达式处理类
 */
public class ElHandler {

    private static final Logger logger = LoggerFactory.getLogger(ElHandler.class);

    // 表达式文本
    private final String expressionText;

    // 表达式允许的变量配置列表
    private final List<ElVariableConfig> elAllowedVariableConfigList = new ArrayList<>();

    // 表达式允许的变量名称Set
    private final Set<String> elAllowedVariableNameSet = new HashSet<>();

    // 表达式有指定的变量名称Set
    private final Set<String> elSpecifiedVariableNameSet = new HashSet<>();

    // 用于写表达式忽略数据的文件输出流
    private Writer elIgnoreDataWriter;

    // 用于写表达式忽略数据文件的线程池
    private ThreadPoolExecutor writeFileTPE;

    // 表达式是否用于忽略数据
    private boolean ignoreData;

    // 表达式配置文件
    private String elConfigFile;

    // 表达式
    private Expression expression;

    /**
     * @param expressionText     表达式文本
     * @param elConfig           表达式配置
     * @param elIgnoreDataWriter 表达式配置
     * @param writeFileTPE       表达式配置
     */
    public ElHandler(String expressionText, ElConfigInterface elConfig, Writer elIgnoreDataWriter, ThreadPoolExecutor writeFileTPE) {
        this.expressionText = expressionText;
        if (StringUtils.isBlank(expressionText)) {
            // 若表达式文本为空，则不创建 Expression 对象
            return;
        }

        if (elConfig == null) {
            throw new JavaCG2RuntimeException("表达式配置不能为空");
        }

        // 检查指定的变量名称是否允许使用
        for (ElAllowedVariableInterface elAllowedVariableEnum : elConfig.getElAllowedVariableEnums()) {
            addElVariableConfig(elAllowedVariableEnum);
        }

        ignoreData = elConfig.isIgnoreData();
        elConfigFile = elConfig.getKey();

        // 创建 Aviator 实例
        AviatorEvaluatorInstance aviatorEvaluatorInstance = AviatorEvaluator.newInstance();
        aviatorEvaluatorInstance.enableSandboxMode();
        aviatorEvaluatorInstance.addFunction(new StringContainsAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringEndsWithAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringEqualsAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringStartsWithAnyFunction());
        aviatorEvaluatorInstance.addFunction(new StringContainsICFunction());
        aviatorEvaluatorInstance.addFunction(new StringEndsWithICFunction());
        aviatorEvaluatorInstance.addFunction(new StringEqualsICFunction());
        aviatorEvaluatorInstance.addFunction(new StringStartsWithICFunction());

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
                logger.error("{} 当前使用的表达式变量名称非法 {}\n允许使用的变量如下\n变量名称\t变量类型\t变量说明\t变量值示例{}", this.getClass().getSimpleName(), variableName, stringBuilder);
                throw new JavaCG2RuntimeException("当前使用的表达式变量名称非法");
            }
            elSpecifiedVariableNameSet.add(variableName);
        }
        this.elIgnoreDataWriter = elIgnoreDataWriter;
        this.writeFileTPE = writeFileTPE;
    }

    /**
     * 增加表达式变量配置
     *
     * @param elAllowedVariableEnum 允许使用表达式的变量枚举
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
     * 检查是否有在表达式中指定变量名称
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

        Object executeResult = expression.execute(map);
        if (!(executeResult instanceof Boolean)) {
            logger.error("{} 表达式执行结果返回值非 {} [{}]", expressionText, Boolean.class.getSimpleName(), expressionText);
            throw new JavaCG2RuntimeException("表达式执行结果返回值非法");
        }
        boolean result = (boolean) executeResult;
        if (result && ignoreData) {
            // 表达式执行结果为true，且当前表达式是需要忽略数据
            writeFileTPE.execute(() -> {
                String mapValueStr = JavaCG2Util.getMapValueStr(map);
                String data = String.format("通过表达式执行结果判断需要忽略当前数据，表达式配置文件： {%s} 表达式： {%s} 表达式使用的变量值： {%s}%s", elConfigFile, expressionText, mapValueStr, JavaCG2Constants.NEW_LINE);
                try {
                    elIgnoreDataWriter.write(data);
                } catch (IOException e) {
                    logger.error("写文件异常 ", e);
                }
            });
        }
        return result;
    }

    /**
     * 生成表达式用于执行的Map
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
