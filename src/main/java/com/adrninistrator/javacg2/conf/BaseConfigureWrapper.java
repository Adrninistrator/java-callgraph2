package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.JavaCG2ConfigPrintConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.comparator.Comparator4MainConfig;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.el.enums.ElStringAnyFunctionEnum;
import com.adrninistrator.javacg2.el.enums.ElStringFunctionTwoArgsEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2Error;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/1/30
 * @description: 配置包装类基类
 */
public abstract class BaseConfigureWrapper {
    private static final Logger logger = LoggerFactory.getLogger(BaseConfigureWrapper.class);

    // 记录使用过当前配置类的简单类名列表
    private final List<String> useThisSimpleClassNameList = new ArrayList<>();

    // 允许使用的配置类名
    private final Set<String> allowedConfigClassNameSet = new HashSet<>();

    /*
        从配置文件中读取的内容
        key     文件名
        value   properties对象
     */
    private Map<String, Properties> propertiesMap = new HashMap<>();

    /*
        记录有被使用的主要配置
        key     文件名
        value   配置参数
     */
    private Map<String, Set<MainConfigInterface>> usedMainConfigMap = new HashMap<>();

    /*
        记录有被使用的其他区分顺序配置
        元素内容  配置文件名（对应 OtherConfigInterface.getKey()）
     */
    private Set<String> usedOtherListConfigSet = new HashSet<>();

    /*
        记录有被使用的其他区分顺序配置
        元素内容  配置文件名（对应 OtherConfigInterface.getKey()）
     */
    private Set<String> usedOtherSetConfigSet = new HashSet<>();

    /*
        记录有被使用的表达式配置
        元素内容  配置文件名（对应 ElConfigInterface.getKey()）
     */
    private Set<String> usedElConfigSet = new HashSet<>();

    /*
        主要配置文件中的参数
        key 参数名
        value 参数值
     */
    private Map<String, Object> mainConfigMap = new HashMap<>();

    /*
        不区分顺序的其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数Set
     */
    private Map<String, Set<String>> otherConfigSetMap = new HashMap<>();

    /*
        区分顺序的其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数List
     */
    private Map<String, List<String>> otherConfigListMap = new HashMap<>();

    /*
        表达式配置
        key 表达式配置文件名
        value 表达式文本
     */
    private Map<String, String> elConfigMap = new HashMap<>();

    /**
     * 默认构造函数，仅使用代码中指定的参数，忽略配置文件中的参数
     */
    public BaseConfigureWrapper() {
        this(true);
    }

    /**
     * 构造函数，指定使用代码中指定的参数，还是使用配置文件中的参数
     *
     * @param onlyUseConfigInJavaCode true: 仅使用代码中指定的参数，忽略配置文件中的参数 false: 使用配置文件中的参数
     */
    public BaseConfigureWrapper(boolean onlyUseConfigInJavaCode) {
        allowedConfigClassNameSet.addAll(Arrays.asList(chooseAllowedConfigClassNames()));
        if (onlyUseConfigInJavaCode) {
            logger.info("仅使用代码中指定的参数，忽略配置文件中的参数");
            // 使用默认的空参数（忽略配置文件中的参数）
            useDefaultEmptyConfig();
        } else {
            logger.info("使用配置文件中的参数");
        }
    }

    // 自定义生成并检查主要配置参数值
    protected abstract Object customGenMainConfigValue(MainConfigInterface mainConfig, String strValue);

    /**
     * 使用默认的空参数（忽略配置文件中的参数）
     * 在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     */
    protected abstract void useDefaultEmptyConfig();

    // 自定义获取默认的参数值
    protected abstract Object customGetDefaultConfig(MainConfigInterface mainConfig);

    // 自定义打印配置参数信息
    protected abstract void customPrintConfigInfo(MarkdownWriter markdownWriter, boolean printAllConfigInfo) throws IOException;

    // 获取主要配置的简单类名
    protected abstract String getMainConfigSCNFromFile(String mainConfigFile);

    /**
     * 选择当前项目使用的不区分顺序的其他配置
     *
     * @return
     */
    protected abstract OtherConfigInterface chooseOtherConfigFileUseSetEnum();

    /**
     * 选择当前项目使用的区分顺序的其他配置
     *
     * @return
     */
    protected abstract OtherConfigInterface chooseOtherConfigFileUseListEnum();

    /**
     * 选择允许使用的配置类名
     *
     * @return
     */
    protected abstract String[] chooseAllowedConfigClassNames();

    // 使用配置时的自定义检查
    protected void customCheckWhenUseConfig() {
    }

    // 检查使用的配置类名是否允许使用
    private void checkAllowedConfigClassName(Object object) {
        String className = object.getClass().getName();
        if (!allowedConfigClassNameSet.contains(className)) {
            logger.error("当前使用的配置类名不允许使用 {}", className);
            throw new JavaCG2RuntimeException("当前使用的配置类名不允许使用");
        }
    }

    // 记录有被使用的主要配置
    private void recordUsedMainConfig(MainConfigInterface mainConfig) {
        // 使用配置时的自定义检查
        customCheckWhenUseConfig();
        Set<MainConfigInterface> usedMainConfigSet = usedMainConfigMap.computeIfAbsent(mainConfig.getFileName(), k -> new HashSet<>());
        usedMainConfigSet.add(mainConfig);
    }

    // 记录有被使用的其他区分顺序配置
    private void recordUsedOtherListConfig(OtherConfigInterface config) {
        // 使用配置时的自定义检查
        customCheckWhenUseConfig();
        usedOtherListConfigSet.add(config.getKey());
    }

    // 记录有被使用的其他不区分顺序配置
    private void recordUsedOtherSetConfig(OtherConfigInterface config) {
        // 使用配置时的自定义检查
        customCheckWhenUseConfig();
        usedOtherSetConfigSet.add(config.getKey());
    }

    // 记录有被使用的表达式配置
    private void recordUsedElConfig(ElConfigInterface config) {
        // 使用配置时的自定义检查
        customCheckWhenUseConfig();
        usedElConfigSet.add(config.getKey());
    }

    /**
     * 设置主要配置文件中指定key的参数，清空指定key已有的参数
     * 需要缓存当前的参数值
     *
     * @param mainConfig
     * @param strValue
     */
    public Object setMainConfig(MainConfigInterface mainConfig, String strValue) {
        return setMainConfig(mainConfig, strValue, true);
    }

    /**
     * 设置主要配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param mainConfig
     * @param strValue
     * @param useConfig  是否是使用当前参数
     */
    public Object setMainConfig(MainConfigInterface mainConfig, String strValue, boolean useConfig) {
        checkAllowedConfigClassName(mainConfig);
        if (strValue == null) {
            throw new JavaCG2Error("配置参数不允许为null");
        }
        Object value;
        try {
            // 生成并检查主要配置参数值
            value = genMainConfigValue(mainConfig, strValue);
        } catch (Exception e) {
            logger.error("处理参数出现异常 {} {} {} {}", mainConfig.getFileName(), mainConfig.getConfigPrintInfo(), mainConfig.getType().getName(), strValue);
            throw new JavaCG2Error("处理参数出现异常");
        }

        if (value == null) {
            logger.error("配置参数非法 {} {} {} {}", mainConfig.getFileName(), mainConfig.getConfigPrintInfo(), mainConfig.getType().getName(), strValue);
            throw new JavaCG2Error("配置参数非法");
        }

        if (!mainConfig.getType().isAssignableFrom(value.getClass())) {
            logger.error("生成的参数值类型与预期的不一致 {} {} {} {}", mainConfig.getFileName(), mainConfig.getConfigPrintInfo(), value.getClass().getName(), mainConfig.getType().getName());
            throw new JavaCG2Error("生成的参数值类型与预期的不一致");
        }
        if (useConfig) {
            logger.info("通过代码设置主要配置的参数 {} {} {}", mainConfig.getFileName(), mainConfig.getConfigPrintInfo(), value);
            // 缓存当前的参数值
            mainConfigMap.put(mainConfig.getKey(), value);
        }
        return value;
    }

    /**
     * 设置不区分顺序的其他配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param otherConfig
     * @param data        若未指定则清空参数
     */
    public void setOtherConfigSet(OtherConfigInterface otherConfig, String... data) {
        setOtherConfigSet(otherConfig, JavaCG2Util.genSetFromArray(data));
    }

    /**
     * 设置不区分顺序的其他配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param otherConfig
     * @param configSet
     */
    public void setOtherConfigSet(OtherConfigInterface otherConfig, Set<String> configSet) {
        checkAllowedConfigClassName(otherConfig);
        if (configSet == null) {
            throw new JavaCG2Error("不允许传入null，只能传入内容为空的Set " + otherConfig.getConfigPrintInfo());
        }
        logger.info("通过代码设置不区分顺序配置的参数 {}", otherConfig.getConfigPrintInfo());
        otherConfigSetMap.put(otherConfig.getKey(), configSet);
    }

    /**
     * 添加不区分顺序的其他配置文件中指定key的参数，保留指定key已有的参数
     *
     * @param otherConfig
     * @param data
     */
    public void addOtherConfigSet(OtherConfigInterface otherConfig, String... data) {
        addOtherConfigSet(otherConfig, JavaCG2Util.genSetFromArray(data));
    }

    /**
     * 添加不区分顺序的其他配置文件中指定key的参数，保留指定key已有的参数
     *
     * @param otherConfig
     * @param configSet
     */
    public void addOtherConfigSet(OtherConfigInterface otherConfig, Set<String> configSet) {
        checkAllowedConfigClassName(otherConfig);
        if (configSet == null) {
            throw new JavaCG2Error("不允许传入null，只能传入内容为空的Set " + otherConfig.getConfigPrintInfo());
        }
        logger.info("通过代码添加不区分顺序配置的参数 {}", otherConfig.getConfigPrintInfo());
        Set<String> existedSet = otherConfigSetMap.get(otherConfig.getKey());
        if (existedSet == null) {
            otherConfigSetMap.put(otherConfig.getKey(), configSet);
            return;
        }
        existedSet.addAll(configSet);
    }

    /**
     * 设置区分顺序的其他配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param otherConfig
     * @param data        若未指定则清空参数
     */
    public void setOtherConfigList(OtherConfigInterface otherConfig, String... data) {
        setOtherConfigList(otherConfig, JavaCG2Util.genListFromArray(data));
    }

    /**
     * 设置区分顺序的其他配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param otherConfig
     * @param configList
     */
    public void setOtherConfigList(OtherConfigInterface otherConfig, List<String> configList) {
        checkAllowedConfigClassName(otherConfig);
        if (configList == null) {
            throw new JavaCG2Error("不允许传入null，只能传入内容为空的List " + otherConfig.getConfigPrintInfo());
        }
        logger.info("通过代码设置区分顺序配置的参数 {}", otherConfig.getConfigPrintInfo());
        otherConfigListMap.put(otherConfig.getKey(), configList);
    }

    /**
     * 添加区分顺序的其他配置文件中指定key的参数，保留指定key已有的参数
     *
     * @param otherConfig
     * @param data
     */
    public void addOtherConfigList(OtherConfigInterface otherConfig, String... data) {
        addOtherConfigList(otherConfig, JavaCG2Util.genListFromArray(data));
    }

    /**
     * 添加区分顺序的其他配置文件中指定key的参数，保留指定key已有的参数
     *
     * @param otherConfig
     * @param configList
     */
    public void addOtherConfigList(OtherConfigInterface otherConfig, List<String> configList) {
        checkAllowedConfigClassName(otherConfig);
        if (configList == null) {
            throw new JavaCG2Error("不允许传入null，只能传入内容为空的List " + otherConfig.getConfigPrintInfo());
        }
        logger.info("通过代码添加区分顺序配置的参数 {}", otherConfig.getConfigPrintInfo());
        List<String> existedList = otherConfigListMap.get(otherConfig.getKey());
        if (existedList == null) {
            List<String> newList = new ArrayList<>();
            JavaCG2Util.addList2List(configList, newList);
            otherConfigListMap.put(otherConfig.getKey(), newList);
            return;
        }
        JavaCG2Util.addList2List(configList, existedList);
    }

    /**
     * 清理不区分顺序的其他配置
     *
     * @param otherConfigs
     */
    protected void clearOtherConfigUseSet(OtherConfigInterface[] otherConfigs) {
        for (OtherConfigInterface otherConfig : otherConfigs) {
            checkAllowedConfigClassName(otherConfig);
            logger.info("区分顺序的配置优先使用代码中指定的参数，若代码中未指定则使用配置文件中的参数 {}", otherConfig.getConfigPrintInfo());
        }
    }

    /**
     * 清理区分顺序的其他配置
     *
     * @param otherConfigs
     */
    protected void clearOtherConfigUseList(OtherConfigInterface[] otherConfigs) {
        for (OtherConfigInterface otherConfig : otherConfigs) {
            checkAllowedConfigClassName(otherConfig);
            logger.info("不区分顺序的配置优先使用代码中指定的参数，若代码中未指定则使用配置文件中的参数 {}", otherConfig.getConfigPrintInfo());
        }
    }

    /**
     * 清理表达式文本
     *
     * @param elConfigs
     */
    protected void clearElConfigText(ElConfigInterface[] elConfigs) {
        for (ElConfigInterface elConfig : elConfigs) {
            checkAllowedConfigClassName(elConfig);
            elConfigMap.put(elConfig.getKey(), "");
        }
    }

    /**
     * 设置指定key对应的表达式文本，清空指定key已有的参数
     *
     * @param elConfig
     * @param elText
     */
    public void setElConfigText(ElConfigInterface elConfig, String elText) {
        checkAllowedConfigClassName(elConfig);
        if (elText == null) {
            throw new JavaCG2Error("不允许传入null，只能传入空字符串 " + elConfig.getConfigPrintInfo());
        }
        logger.info("通过代码设置表达式配置 {}", elConfig.getConfigPrintInfo());
        elConfigMap.put(elConfig.getKey(), elText);
    }

    /**
     * 设置表达式固定返回true
     *
     * @param elConfig 表达式枚举
     */
    public void setElConfigFixedTrue(ElConfigInterface elConfig) {
        setElConfigText(elConfig, Boolean.TRUE.toString());
    }

    /**
     * 设置表达式固定返回false
     *
     * @param elConfig 表达式枚举
     */
    public void setElConfigFixedFalse(ElConfigInterface elConfig) {
        setElConfigText(elConfig, Boolean.FALSE.toString());
    }

    /**
     * 设置类似 StringUtils.xxxAny 功能的字符串比较表达式
     *
     * @param elConfig              表达式枚举
     * @param stringAnyFunctionEnum 字符串比较方式枚举
     * @param elVariable            表达式变量
     * @param args                  需要比较的字符串值，一个或多个
     */
    public void setElConfigStringAny(ElConfigInterface elConfig, ElStringAnyFunctionEnum stringAnyFunctionEnum, ElAllowedVariableInterface elVariable, String... args) {
        if (ArrayUtils.isEmpty(args)) {
            throw new JavaCG2RuntimeException("未指定需要比较的字符串值");
        }
        // 检查表达式变量是否允许使用
        checkElAllowedVariable(elConfig, elVariable);

        StringBuilder elText = new StringBuilder();
        elText.append(stringAnyFunctionEnum.getName()).append("(").append(elVariable.getVariableName()).append(",");
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (arg == null) {
                throw new JavaCG2RuntimeException("需要比较的字符串值不允许为空");
            }
            if (i > 0) {
                elText.append(",");
            }
            elText.append(arg);
        }
        elText.append(");");
        setElConfigText(elConfig, elText.toString());
    }

    /**
     * 设置比较字符串的表达式
     *
     * @param elConfig                    表达式枚举
     * @param elStringFunctionTwoArgsEnum 字符串比较方式枚举
     * @param elVariable                  表达式变量
     * @param arg                         需要比较的字符串值
     */
    public void setElConfigStringAny(ElConfigInterface elConfig, ElStringFunctionTwoArgsEnum elStringFunctionTwoArgsEnum, ElAllowedVariableInterface elVariable, String arg) {
        if (StringUtils.isBlank(arg)) {
            throw new JavaCG2RuntimeException("未指定需要比较的字符串值");
        }
        // 检查表达式变量是否允许使用
        checkElAllowedVariable(elConfig, elVariable);

        String elText = elStringFunctionTwoArgsEnum.getName() + "(" + elVariable.getVariableName() + "," + arg + ");";
        setElConfigText(elConfig, elText);
    }

    // 检查表达式变量是否允许使用
    private void checkElAllowedVariable(ElConfigInterface elConfig, ElAllowedVariableInterface elVariable) {
        for (ElAllowedVariableInterface tmpElAllowedVariable : elConfig.getElAllowedVariableEnums()) {
            if (tmpElAllowedVariable.getVariableName().equals(elVariable.getVariableName())) {
                return;
            }
        }
        logger.error("当前表达式不允许使用该变量 {} {}", elConfig.getKey(), elVariable.getVariableName());
        throw new JavaCG2RuntimeException("当前表达式不允许使用该变量");
    }

    /**
     * 获取主要配置文件中的参数，或通过代码添加的参数，实际使用配置参数
     *
     * @param mainConfig
     * @return
     */
    public <T> T getMainConfig(MainConfigInterface mainConfig) {
        return getMainConfig(mainConfig, true);
    }

    /**
     * 获取主要配置文件中的参数，或通过代码添加的参数
     *
     * @param mainConfig
     * @param useConfig  true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> T getMainConfig(MainConfigInterface mainConfig, boolean useConfig) {
        String key = mainConfig.getKey();
        // 优先获取通过代码添加的参数
        Object value = mainConfigMap.get(key);
        if (value != null) {
            if (useConfig) {
                // 判断不允许为空的参数是否有指定值
                if (mainConfig.notBlank() && value instanceof String && StringUtils.isBlank((String) value)) {
                    logger.error("需要使用的配置参数未在代码中指定 {} {}", mainConfig.getFileName(), mainConfig.getConfigPrintInfo());
                    throw new JavaCG2Error("需要使用的配置参数未在代码中指定: " + mainConfig.getFileName() + " " + mainConfig.getConfigPrintInfo());
                }

                // 记录有被使用的主要配置
                recordUsedMainConfig(mainConfig);
            }
            return (T) value;
        }

        // 读取配置文件
        String configFileName = mainConfig.getFileName();
        Properties properties = propertiesMap.get(configFileName);
        if (properties == null) {
            try (BufferedReader reader = JavaCG2FileUtil.genBufferedReader(JavaCG2FileUtil.getFileInputStream(JavaCG2Util.getInputRootPath() + configFileName))) {
                properties = new Properties();
                properties.load(reader);
                propertiesMap.put(configFileName, properties);
            } catch (Exception e) {
                logger.error("error ", e);
                throw new JavaCG2Error("读取配置文件出错: " + configFileName);
            }
        }

        // 获取配置文件中的参数
        String strValue = properties.getProperty(key);
        if (useConfig) {
            if (mainConfig.notBlank() && StringUtils.isBlank(strValue)) {
                logger.error("需要使用的配置参数未在配置文件中指定 {} {}", mainConfig.getFileName(), mainConfig.getConfigPrintInfo());
                throw new JavaCG2Error("需要使用的配置参数未在配置文件中指定: " + mainConfig.getFileName() + " " + mainConfig.getConfigPrintInfo());
            }
            // 记录有被使用的主要配置
            recordUsedMainConfig(mainConfig);
        }

        if (strValue == null) {
            // 获取默认的参数值
            strValue = String.valueOf(getDefaultConfig(mainConfig));
        }
        // 设置参数值，仅当useConfig为true时，才需要缓存参数值
        return (T) setMainConfig(mainConfig, strValue, useConfig);
    }

    /**
     * 获取不区分顺序的其他配置文件中的参数，或通过代码添加的参数，需要实际使用配置参数
     *
     * @param otherConfig
     * @return
     */
    public Set<String> getOtherConfigSet(OtherConfigInterface otherConfig) {
        return getOtherConfigSet(otherConfig, true);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数，不区分顺序
     *
     * @param otherConfig
     * @param useConfig   true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    public Set<String> getOtherConfigSet(OtherConfigInterface otherConfig, boolean useConfig) {
        String configFileName = otherConfig.getKey();
        // 优先获取通过代码添加的参数
        Set<String> configSet = otherConfigSetMap.get(configFileName);
        if (configSet != null) {
            if (useConfig) {
                // 记录有被使用的其他配置
                recordUsedOtherSetConfig(otherConfig);
            }
            return configSet;
        }

        // 获取其他配置文件中的参数
        configSet = JavaCG2FileUtil.readFile2Set(JavaCG2Util.getInputRootPath() + configFileName);
        // 将配置文件中的参数记录到内存中，用于后续使用
        otherConfigSetMap.put(configFileName, configSet);
        if (useConfig) {
            // 记录有被使用的其他配置
            recordUsedOtherSetConfig(otherConfig);
        }
        return configSet;
    }

    /**
     * 获取区分顺序的其他配置文件中的参数，或通过代码添加的参数，需要实际使用配置参数
     *
     * @param otherConfig 参数key枚举
     * @return
     */
    public List<String> getOtherConfigList(OtherConfigInterface otherConfig) {
        return getOtherConfigList(otherConfig, true);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数，区分顺序
     *
     * @param otherConfig 参数key枚举
     * @param useConfig   true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    public List<String> getOtherConfigList(OtherConfigInterface otherConfig, boolean useConfig) {
        String configFileName = otherConfig.getKey();
        // 优先获取通过代码添加的参数
        List<String> configList = otherConfigListMap.get(configFileName);
        if (configList != null) {
            if (useConfig) {
                // 记录有被使用的其他配置
                recordUsedOtherListConfig(otherConfig);
            }
            return configList;
        }

        // 获取其他配置文件中的参数
        configList = JavaCG2FileUtil.readFile2List(JavaCG2Util.getInputRootPath() + configFileName);
        // 将配置文件中的参数记录到内存中，用于后续使用
        otherConfigListMap.put(configFileName, configList);
        if (useConfig) {
            // 记录有被使用的其他配置
            recordUsedOtherListConfig(otherConfig);
        }
        return configList;
    }

    // 生成并检查主要配置参数值
    private Object genMainConfigValue(MainConfigInterface mainConfig, String strValue) {
        // 自定义生成并检查主要配置参数值
        Object object = customGenMainConfigValue(mainConfig, strValue);
        if (object != null) {
            return object;
        }

        if (String.class == mainConfig.getType()) {
            return strValue;
        }
        if (Boolean.class == mainConfig.getType()) {
            return Boolean.valueOf(strValue);
        }
        if (Integer.class == mainConfig.getType()) {
            return Integer.valueOf(strValue);
        }

        // 不可能执行到这里
        logger.error("未知情况 {} {}", mainConfig, strValue);
        throw new JavaCG2RuntimeException("未知情况 " + mainConfig + " " + strValue);
    }

    // 清空指定的主要参数
    protected void clearMainConfigs(MainConfigInterface[] mainConfigs) {
        for (MainConfigInterface mainConfig : mainConfigs) {
            // 获取默认的参数值
            Object value = getDefaultConfig(mainConfig);
            mainConfigMap.put(mainConfig.getKey(), value);
        }
    }

    // 获取默认的参数值
    private Object getDefaultConfig(MainConfigInterface mainConfig) {
        // 自定义获取默认的参数值
        Object object = customGetDefaultConfig(mainConfig);
        if (object != null) {
            return object;
        }

        if (Boolean.class == mainConfig.getType()) {
            return Boolean.FALSE;
        }
        if (Integer.class == mainConfig.getType()) {
            return 0;
        }
        return "";
    }

    /**
     * 获取表达式文本
     *
     * @param elConfig 参数key枚举
     * @return
     */
    public String getElConfigText(ElConfigInterface elConfig) {
        return getElConfigText(elConfig, true);
    }

    /**
     * 获取表达式文本
     *
     * @param elConfig  参数key枚举
     * @param useConfig true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    public String getElConfigText(ElConfigInterface elConfig, boolean useConfig) {
        String configFileName = elConfig.getKey();
        // 优先获取通过代码添加的参数
        String elText = elConfigMap.get(configFileName);
        if (elText != null) {
            if (useConfig) {
                // 记录有被使用的其他配置
                recordUsedElConfig(elConfig);
            }
            return elText;
        }

        // 获取配置文件中的参数
        List<String> list = JavaCG2FileUtil.readFile2List(JavaCG2Util.getInputRootPath() + configFileName, "##");
        elText = StringUtils.join(list, "");
        if (StringUtils.isBlank(elText)) {
            elText = "";
        }
        // 将配置文件中的参数记录到内存中，用于后续使用
        elConfigMap.put(configFileName, elText);
        if (useConfig) {
            // 记录有被使用的其他配置
            recordUsedElConfig(elConfig);
        }
        return elText;
    }

    /**
     * 拷贝数据
     *
     * @return
     */
    protected BaseConfigureWrapper baseCopy() {
        BaseConfigureWrapper copied;
        try {
            Class<?> clazz = this.getClass();
            Constructor<?> constructor = clazz.getConstructor(boolean.class);
            copied = (BaseConfigureWrapper) constructor.newInstance(false);
        } catch (Exception e) {
            logger.error("创建拷贝数据的对象失败 {} ", this.getClass().getName(), e);
            throw new JavaCG2RuntimeException("创建拷贝数据的对象失败");
        }

        copied.propertiesMap = new HashMap<>(this.propertiesMap);
        copied.usedMainConfigMap = new HashMap<>(this.usedMainConfigMap);
        copied.usedOtherListConfigSet = new HashSet<>(this.usedOtherListConfigSet);
        copied.usedOtherSetConfigSet = new HashSet<>(this.usedOtherSetConfigSet);
        copied.mainConfigMap = new HashMap<>(this.mainConfigMap);
        // 以下的Map的value为List或Set，也需要进行复制
        copied.otherConfigSetMap = new HashMap<>();
        for (Map.Entry<String, Set<String>> entry : this.otherConfigSetMap.entrySet()) {
            copied.otherConfigSetMap.put(entry.getKey(), new HashSet<>(entry.getValue()));
        }
        copied.otherConfigListMap = new HashMap<>();
        for (Map.Entry<String, List<String>> entry : this.otherConfigListMap.entrySet()) {
            copied.otherConfigListMap.put(entry.getKey(), new ArrayList<>(entry.getValue()));
        }
        copied.elConfigMap = new HashMap<>(this.elConfigMap);
        return copied;
    }

    /**
     * 执行完毕时打印当前使用的配置信息
     *
     * @param simpleClassName
     * @param outputDirPath
     * @param fileName
     */
    public void printUsedConfigInfo(String simpleClassName, String outputDirPath, String fileName) {
        String configMdFilePath = JavaCG2Util.addSeparator4FilePath(outputDirPath) + fileName;
        logger.info("{} 使用的配置参数信息保存到以下文件 {}", simpleClassName, configMdFilePath);
        // 打印使用的配置参数信息
        printConfigInfo(simpleClassName, configMdFilePath, false);
    }

    /**
     * 执行完毕时打印当前所有配置信息
     *
     * @param simpleClassName
     * @param outputDirPath
     * @param fileName
     */
    public void printAllConfigInfo(String simpleClassName, String outputDirPath, String fileName) {
        String configMdFilePath = JavaCG2Util.addSeparator4FilePath(outputDirPath) + fileName;
        logger.info("{} 所有配置参数信息保存到以下文件 {}", simpleClassName, configMdFilePath);
        // 打印使用的配置参数信息
        printConfigInfo(simpleClassName, configMdFilePath, true);
    }

    /**
     * 打印配置参数信息
     *
     * @param simpleClassName
     * @param configMdFilePath
     * @param printAllConfigInfo true: 打印所有的配置参数信息 false: 打印使用的配置参数信息
     */
    private void printConfigInfo(String simpleClassName, String configMdFilePath, boolean printAllConfigInfo) {
        // 当前文件可能会写多次，最后一次写的内容覆盖前面的内容，所有使用过当前配置类的类名都会被记录
        if (!useThisSimpleClassNameList.contains(simpleClassName)) {
            useThisSimpleClassNameList.add(simpleClassName);
        }
        try (MarkdownWriter markdownWriter = new MarkdownWriter(configMdFilePath, true)) {
            // 添加公共的说明
            addCommonDesc(markdownWriter);
            if (!printAllConfigInfo) {
                // 打印使用的配置参数信息，先打印当前有使用的配置参数
                printUsedConfig(markdownWriter, StringUtils.join(useThisSimpleClassNameList, " "));
            }

            // 打印主要的配置信息
            markdownWriter.addTitle(1, JavaCG2ConfigPrintConstants.MAIN_CONFIG);

            // 自定义打印配置参数信息
            customPrintConfigInfo(markdownWriter, printAllConfigInfo);
        } catch (Exception e) {
            logger.error("{} error ", simpleClassName, e);
        }
    }

    // 打印有使用的配置参数key及描述
    private void printUsedConfig(MarkdownWriter markdownWriter, String simpleClassNames) throws IOException {
        markdownWriter.addTitle(1, "使用过当前配置类的简单类名列表");
        markdownWriter.addLineWithNewLine(simpleClassNames);

        if (!usedMainConfigMap.isEmpty()) {
            // 当主要配置参数有使用时，打印有使用的主要的配置key及描述
            markdownWriter.addTitle(1, "当前有使用的主要配置参数");
            List<String> usedMainConfigFileList = new ArrayList<>(usedMainConfigMap.keySet());
            Collections.sort(usedMainConfigFileList);
            for (String usedMainConfigFile : usedMainConfigFileList) {
                markdownWriter.addTitle(2, usedMainConfigFile);

                String mainConfigEnumSCN = getMainConfigSCNFromFile(usedMainConfigFile);
                markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS);
                markdownWriter.addLineWithNewLine(mainConfigEnumSCN);

                List<MainConfigInterface> usedMainConfigKeyList = new ArrayList<>(usedMainConfigMap.get(usedMainConfigFile));
                usedMainConfigKeyList.sort(Comparator4MainConfig.getInstance());

                markdownWriter.addTableHead(JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_KEY, JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_ENUM_NAME,
                        JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_DESC);

                for (MainConfigInterface usedMainConfig : usedMainConfigKeyList) {
                    markdownWriter.addTableBody(usedMainConfig.getKey(), usedMainConfig.getEnumName(), StringUtils.join(usedMainConfig.getDescriptions(), " "));
                }
                markdownWriter.addEmptyLine();
            }
        }

        // 打印其他配置信息
        OtherConfigInterface otherConfigFileUseListEnum = chooseOtherConfigFileUseListEnum();
        markdownWriter.addTitle(1, "当前有使用的区分顺序的其他配置信息");
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS);
        markdownWriter.addLineWithNewLine(otherConfigFileUseListEnum.getClass().getSimpleName());

        markdownWriter.addTableHead(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_KEY, JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS,
                JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_DESC);
        List<String> usedOtherListConfigFileList = new ArrayList<>(usedOtherListConfigSet);
        Collections.sort(usedOtherListConfigFileList);
        for (String usedOtherConfigFile : usedOtherListConfigFileList) {
            OtherConfigInterface tmpOtherConfigFileUseListEnum = otherConfigFileUseListEnum.getFromKey(usedOtherConfigFile);
            markdownWriter.addTableBody(usedOtherConfigFile, tmpOtherConfigFileUseListEnum.getEnumName(), StringUtils.join(tmpOtherConfigFileUseListEnum.getDescriptions(),
                    " "));
        }
        markdownWriter.addEmptyLine();

        OtherConfigInterface otherConfigFileUseSetEnum = chooseOtherConfigFileUseSetEnum();
        markdownWriter.addTitle(1, "当前有使用的不区分顺序的其他配置信息");
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS);
        markdownWriter.addLineWithNewLine(otherConfigFileUseSetEnum.getClass().getSimpleName());

        markdownWriter.addTableHead(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_KEY, JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS,
                JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_DESC);
        List<String> usedOtherSetConfigFileList = new ArrayList<>(usedOtherSetConfigSet);
        Collections.sort(usedOtherSetConfigFileList);
        for (String usedOtherConfigFile : usedOtherSetConfigFileList) {
            OtherConfigInterface tmpOtherConfigFileUseSetEnum = otherConfigFileUseSetEnum.getFromKey(usedOtherConfigFile);
            markdownWriter.addTableBody(usedOtherConfigFile, tmpOtherConfigFileUseSetEnum.getEnumName(), StringUtils.join(tmpOtherConfigFileUseSetEnum.getDescriptions(), " "));
        }
        markdownWriter.addEmptyLine();
    }

    // 打印主要的配置信息
    protected void printMainConfigInfo(MarkdownWriter markdownWriter, MainConfigInterface[] configs, boolean printAllConfigInfo) throws IOException {
        if (!printAllConfigInfo && usedMainConfigMap.isEmpty()) {
            // 打印使用的配置参数信息，且主要的配置参数未使用，不打印
            return;
        }
        MainConfigInterface firstMainConfig = configs[0];
        markdownWriter.addTitle(2, firstMainConfig.getFileName());
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS);
        markdownWriter.addLineWithNewLine(firstMainConfig.getClass().getSimpleName());
        markdownWriter.addTableHead(JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_KEY, JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_ENUM_NAME,
                JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_DESC, JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_VALUE);

        for (MainConfigInterface mainConfig : configs) {
            if (!printAllConfigInfo) {
                // 打印使用的配置参数信息
                Set<MainConfigInterface> usedMainConfigSet = usedMainConfigMap.get(mainConfig.getFileName());
                if (usedMainConfigSet == null || !usedMainConfigSet.contains(mainConfig)) {
                    // 当前配置参数未使用，不打印
                    continue;
                }
            }
            // 执行打印主要的配置信息
            doPrintMainConfigInfo(markdownWriter, mainConfig.getKey(), mainConfig.getEnumName(), mainConfig.getDescriptions(), getMainConfig(mainConfig, false));
        }
        // 最后写入空行
        markdownWriter.addEmptyLine();
    }

    // 打印区分顺序的其他配置信息
    protected void printOtherListConfigInfo(MarkdownWriter markdownWriter, OtherConfigInterface[] otherConfigs, boolean printAllConfigInfo) throws IOException {
        for (int i = 0; i < otherConfigs.length; i++) {
            OtherConfigInterface currentConfig = otherConfigs[i];
            if (!printAllConfigInfo && !usedOtherListConfigSet.contains(currentConfig.getKey())) {
                // 打印使用的配置参数信息时，当前配置参数未使用，不打印
                continue;
            }
            // 执行打印区分顺序的配置信息
            doPrintListConfigInfo(markdownWriter, i, currentConfig.getKey(), currentConfig.getClass().getSimpleName(), currentConfig.getEnumName(),
                    currentConfig.getDescriptions(),
                    getOtherConfigList(currentConfig, false));
        }
    }

    // 打印不区分顺序的其他配置信息
    protected void printOtherSetConfigInfo(MarkdownWriter markdownWriter, OtherConfigInterface[] otherConfigs, boolean printAllConfigInfo) throws IOException {
        for (int i = 0; i < otherConfigs.length; i++) {
            OtherConfigInterface currentConfig = otherConfigs[i];
            if (!printAllConfigInfo && !usedOtherSetConfigSet.contains(currentConfig.getKey())) {
                // 打印使用的配置参数信息时，当前配置参数未使用，不打印
                continue;
            }
            // 执行打印不区分顺序的配置信息
            doPrintSetConfigInfo(markdownWriter, i, currentConfig.getKey(), currentConfig.getClass().getSimpleName(), currentConfig.getEnumName(),
                    currentConfig.getDescriptions(),
                    getOtherConfigSet(currentConfig, false));
        }
    }

    // 执行打印主要配置信息
    public void doPrintMainConfigInfo(MarkdownWriter markdownWriter, String key, String enumName, String[] descriptions, Object value) throws IOException {
        // 写入配置信息
        String strValue = JavaCG2Util.getObjectPrintValue(value);
        markdownWriter.addTableBody(key, enumName, StringUtils.join(descriptions, " "), (value == null ? "" : strValue));
    }

    // 执行打印区分顺序的配置信息
    public void doPrintListConfigInfo(MarkdownWriter markdownWriter, int index, String key, String configEnumSCN, String enumName, String[] descriptions,
                                      List<String> configList) throws IOException {
        if (index == 0) {
            // 写入配置文件名
            markdownWriter.addTitle(1, JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_LIST);
        }
        // 写入配置信息
        markdownWriter.addTitle(2, key);
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS_AND_NAME);
        markdownWriter.addLineWithNewLine(configEnumSCN + JavaCG2Constants.FLAG_DOT + enumName);
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_DESC);
        for (String description : descriptions) {
            markdownWriter.addLineWithNewLine(description);
        }
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_VALUE);
        markdownWriter.addCodeBlock();
        for (String configValue : configList) {
            markdownWriter.addLine(configValue);
        }
        markdownWriter.addCodeBlock();
    }

    // 执行打印不区分顺序的配置信息
    public void doPrintSetConfigInfo(MarkdownWriter markdownWriter, int index, String key, String configEnumSCN, String enumName, String[] descriptions,
                                     Set<String> configSet) throws
            IOException {
        if (index == 0) {
            // 写入配置文件名
            markdownWriter.addTitle(1, JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_SET);
        }
        markdownWriter.addTitle(2, key);
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_FILE_ENUM_CLASS_AND_NAME);
        markdownWriter.addLineWithNewLine(configEnumSCN + JavaCG2Constants.FLAG_DOT + enumName);
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_DESC);
        for (String description : descriptions) {
            markdownWriter.addLineWithNewLine(description);
        }
        markdownWriter.addListWithNewLine(JavaCG2ConfigPrintConstants.CONFIG_FLAG_CONF_VALUE);
        markdownWriter.addCodeBlock();
        List<String> configValueList = new ArrayList<>(configSet);
        // 排序后打印
        Collections.sort(configValueList);
        for (String configValue : configValueList) {
            markdownWriter.addLine(configValue);
        }
        markdownWriter.addCodeBlock();
    }

    // 添加公共的说明
    public void addCommonDesc(MarkdownWriter markdownWriter) throws IOException {
        markdownWriter.addTitle(1, "说明");
        markdownWriter.addLineWithNewLine("当前文件中的配置文件只有基本的说明，各配置文件的详细说明请打开对应的配置文件查看");
        markdownWriter.addLineWithNewLine("每个配置参数可以通过配置文件或对应的枚举进行修改，效果相同");
    }
}
