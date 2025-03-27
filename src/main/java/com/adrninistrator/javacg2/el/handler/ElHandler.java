package com.adrninistrator.javacg2.el.handler;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
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
import com.adrninistrator.javacg2.el.util.ElUtil;
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
import java.util.HashMap;
import java.util.HashSet;
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

    /*
        表达式允许的变量名称Map
        key     变量名称
        value   是否为 ｛名称前缀}{数字} 的形式
     */
    private final Map<String, Boolean> elAllowedVariableNameMap = new HashMap<>();

    // 表达式有指定的变量名称Set
    private final Set<String> elSpecifiedVariableNameSet = new HashSet<>();

    /*
        名称前缀及对应的数字Map
        key     名称前缀
        value   对应的数字集合
     */
    private final Map<String, Set<Integer>> prefixWithNumMap = new HashMap<>();

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

    // 是否调试模式
    private boolean debugMode = false;

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
        AviatorEvaluatorInstance aviatorEvaluatorInstance = genAviatorEvaluatorInstance();
        // 根据表达式文本编译表达式对象
        logger.info("表达式文本 {} [{}]", elConfigFile, expressionText);
        expression = aviatorEvaluatorInstance.compile(expressionText, true);
        // 检查表达式文本中使用的变量名称
        checkElVariableName();

        this.elIgnoreDataWriter = elIgnoreDataWriter;
        this.writeFileTPE = writeFileTPE;
    }

    // 创建 Aviator 实例
    private AviatorEvaluatorInstance genAviatorEvaluatorInstance() {
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
        return aviatorEvaluatorInstance;
    }

    // 检查表达式文本中使用的变量名称
    private void checkElVariableName() {
        // 遍历并处理表达式文本中出现的变量名称
        for (String variableName : expression.getVariableNames()) {
            doCheckElVariableName(variableName);
        }
    }

    private void doCheckElVariableName(String variableName) {
        for (Map.Entry<String, Boolean> entry : elAllowedVariableNameMap.entrySet()) {
            String allowedVariableName = entry.getKey();
            boolean prefixWithNum = entry.getValue();
            if (!prefixWithNum) {
                if (allowedVariableName.equals(variableName)) {
                    logger.debug("使用完整的表达式变量名称 {}", variableName);
                    elSpecifiedVariableNameSet.add(variableName);
                    return;
                }
                continue;
            }
            if (variableName.startsWith(allowedVariableName)) {
                if (variableName.equals(allowedVariableName)) {
                    logger.error("当前使用的 ｛名称前缀}{数字} 形式表达式变量名称非法，需要在后面指定 {数字} {}", variableName);
                    throw new JavaCG2RuntimeException("当前使用的 ｛名称前缀}{数字} 形式表达式变量名称非法，需要在后面指定 {数字}");
                }
                String numStr = StringUtils.substringAfter(variableName, allowedVariableName);
                if (!JavaCG2Util.isNumStr(numStr)) {
                    logger.error("当前使用的 ｛名称前缀}{数字} 形式表达式变量名称非法，后面指定的内容需要是 {数字} {}", variableName);
                    throw new JavaCG2RuntimeException("当前使用的 ｛名称前缀}{数字} 形式表达式变量名称非法，后面指定的内容需要是 {数字}");
                }
                int num = Integer.parseInt(numStr);
                if (num < 0) {
                    logger.error("当前使用的 ｛名称前缀}{数字} 形式表达式变量名称非法，后面指定的 {数字} 需要是正整数 {}", variableName);
                    throw new JavaCG2RuntimeException("当前使用的 ｛名称前缀}{数字} 形式表达式变量名称非法，后面指定的 {数字} 需要是正整数");
                }
                // 记录 ｛名称前缀}{数字} 形式表达式变量
                Set<Integer> numSet = prefixWithNumMap.computeIfAbsent(allowedVariableName, k -> new HashSet<>());
                numSet.add(num);
                elSpecifiedVariableNameSet.add(variableName);
                return;
            }
        }

        logger.error("当前使用的表达式变量名称非法 {} 允许使用的变量查看表达式配置文件 {}", variableName, elConfigFile);
        throw new JavaCG2RuntimeException("当前使用的表达式变量名称非法");
    }

    /**
     * 增加表达式变量配置
     *
     * @param elAllowedVariableEnum 允许使用表达式的变量枚举
     */
    protected void addElVariableConfig(ElAllowedVariableInterface elAllowedVariableEnum) {
        if (elAllowedVariableNameMap.putIfAbsent(elAllowedVariableEnum.getVariableName(), elAllowedVariableEnum.isPrefixWithNum()) != null) {
            throw new JavaCG2RuntimeException("添加了重复的变量名称 " + elAllowedVariableEnum.getVariableName());
        }
    }

    /**
     * 检查是否有在表达式中指定变量名称
     * 表达式变量不是 ｛名称前缀}{数字} 的形式
     *
     * @param elVariable 变量枚举
     * @return
     */
    public boolean checkVariableNameSpecified(ElAllowedVariableInterface elVariable) {
        // 调试模式时所有变量都使用
        return debugMode || elSpecifiedVariableNameSet.contains(elVariable.getVariableName());
    }

    /**
     * 检查是否有在表达式中指定变量名称
     * 表达式变量是 ｛名称前缀}{数字} 的形式
     *
     * @param elVariable 变量枚举
     * @return
     */
    public boolean checkVariableNamePrefixWithNumSpecified(ElAllowedVariableInterface elVariable) {
        // 调试模式时所有变量都使用
        return debugMode || prefixWithNumMap.containsKey(elVariable.getVariableName());
    }

    /**
     * 检查是否有在表达式中指定变量名称
     * 表达式变量是 ｛名称前缀}{数字} 的形式
     *
     * @param elVariable 变量枚举
     * @param num        数字
     * @return
     */
    public boolean checkVariableNamePrefixWithNumSpecified(ElAllowedVariableInterface elVariable, int num) {
        if (debugMode) {
            // 调试模式时所有变量都使用
            return true;
        }

        Set<Integer> numSet = prefixWithNumMap.get(elVariable.getVariableName());
        if (numSet == null) {
            return false;
        }
        return numSet.contains(num);
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

        Object executeResult;
        try {
            executeResult = expression.execute(map);
            if (debugMode) {
                logger.info("执行表达式 配置文件 [{}] 文本 [{}] 执行结果 [{}] 变量信息： {}", elConfigFile, expressionText, executeResult, JavaCG2Util.getMapValueStr(map));
            }
        } catch (Exception e) {
            logger.error("表达式执行失败 配置文件 [{}] 文本 [{}] 变量信息： [{}]", elConfigFile, expressionText, JavaCG2Util.getMapValueStr(map));
            throw new JavaCG2RuntimeException("表达式执行失败");
        }
        if (!(executeResult instanceof Boolean)) {
            logger.error("表达式 配置文件 [{}] 文本 [{}] 执行结果返回值类型非 {} [{}]", elConfigFile, expressionText, Boolean.class.getSimpleName(), executeResult);
            throw new JavaCG2RuntimeException("表达式执行结果返回值非法");
        }
        boolean result = (boolean) executeResult;
        if (result && ignoreData && !ElUtil.checkRunInCheckerFlag()) {
            // 表达式执行结果为true，且当前表达式是需要忽略数据，且没有执行用于检测的表达式标志
            writeFileTPE.execute(() -> {
                String mapValueStr = JavaCG2Util.getMapValueStr(map);
                String data = String.format("通过表达式执行结果判断需要忽略当前数据，表达式配置文件： {%s} 表达式： {%s} 表达式使用的变量值： %s%s", elConfigFile, expressionText, mapValueStr, JavaCG2Constants.NEW_LINE);
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

    public void setDebugMode(boolean debugMode) {
        this.debugMode = debugMode;
    }
}
