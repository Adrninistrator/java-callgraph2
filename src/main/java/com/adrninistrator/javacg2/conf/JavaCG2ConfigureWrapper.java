package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2Error;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description: 配置参数包装类
 */
public class JavaCG2ConfigureWrapper {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ConfigureWrapper.class);

    /*
        主要配置文件中的参数
        key 参数名
        value 参数值
     */
    private final Map<String, String> configMap = new HashMap<>();

    /*
        其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数Set
     */
    private final Map<String, Set<String>> otherConfigSetMap = new HashMap<>();

    /*
        其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数List
     */
    private final Map<String, List<String>> otherConfigListMap = new HashMap<>();

    /**
     * 默认构造函数，忽略配置文件中的参数
     */
    public JavaCG2ConfigureWrapper() {
        this(true);
    }

    /**
     * @param useDefaultEmptyConfigFlag true: 使用默认的空参数（忽略配置文件中的参数） false: 使用配置文件中的参数
     */
    public JavaCG2ConfigureWrapper(boolean useDefaultEmptyConfigFlag) {
        if (useDefaultEmptyConfigFlag) {
            // 使用默认的空参数（忽略配置文件中的参数）
            useDefaultEmptyConfig();
        }
    }

    /**
     * 使用默认的空参数（忽略配置文件中的参数）
     * 在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     */
    public void useDefaultEmptyConfig() {
        for (JavaCG2ConfigKeyEnum javaCG2ConfigKeyEnum : JavaCG2ConfigKeyEnum.values()) {
            configMap.put(javaCG2ConfigKeyEnum.getKey(), "");
        }
        for (JavaCG2OtherConfigFileUseSetEnum javaCG2OtherConfigFileUseSetEnum : JavaCG2OtherConfigFileUseSetEnum.values()) {
            otherConfigSetMap.put(javaCG2OtherConfigFileUseSetEnum.getFileName(), Collections.emptySet());
        }
        for (JavaCG2OtherConfigFileUseListEnum javaCG2OtherConfigFileUseListEnum : JavaCG2OtherConfigFileUseListEnum.values()) {
            otherConfigListMap.put(javaCG2OtherConfigFileUseListEnum.getFileName(), Collections.emptyList());
        }
    }

    /**
     * 设置配置文件中的参数
     *
     * @param javaCG2ConfigKeyEnum
     * @param value
     */
    public void setConfig(JavaCG2ConfigKeyEnum javaCG2ConfigKeyEnum, String value) {
        if (value == null) {
            return;
        }

        configMap.put(javaCG2ConfigKeyEnum.getKey(), value);
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCG2OtherConfigFileUseSetEnum
     * @param data
     */
    public void setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum javaCG2OtherConfigFileUseSetEnum, String... data) {
        setOtherConfigSet(javaCG2OtherConfigFileUseSetEnum, JavaCG2Util.genSetFromArray(data));
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCG2OtherConfigFileUseSetEnum
     * @param configSet
     */
    public void setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum javaCG2OtherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            throw new JavaCG2Error("不允许传入null，只能传入内容为空的Set");
        }
        otherConfigSetMap.put(javaCG2OtherConfigFileUseSetEnum.getFileName(), configSet);
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCG2OtherConfigFileUseListEnum
     * @param data
     */
    public void setOtherConfigList(JavaCG2OtherConfigFileUseListEnum javaCG2OtherConfigFileUseListEnum, String... data) {
        setOtherConfigList(javaCG2OtherConfigFileUseListEnum, JavaCG2Util.genListFromArray(data));
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCG2OtherConfigFileUseListEnum
     * @param configList
     */
    public void setOtherConfigList(JavaCG2OtherConfigFileUseListEnum javaCG2OtherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            throw new JavaCG2Error("不允许传入null，只能传入内容为空的List");
        }
        otherConfigListMap.put(javaCG2OtherConfigFileUseListEnum.getFileName(), configList);
    }

    /**
     * 设置需要处理所有的类
     */
    public void setPackageUseAll() {
        setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_PACKAGES, new HashSet<>());
    }

    /**
     * 获取配置文件中的参数，或通过代码添加的参数
     *
     * @param properties
     * @param javaCG2ConfigKeyEnum
     * @return
     */
    public String getConfig(Properties properties, JavaCG2ConfigKeyEnum javaCG2ConfigKeyEnum, boolean printLog) {
        String key = javaCG2ConfigKeyEnum.getKey();
        // 优先获取通过代码添加的参数
        String value = configMap.get(key);
        if (value != null) {
            if (printLog) {
                logger.info("使用通过代码添加的参数 [{}] {}", key, value);
            }
            return value;
        }

        if (properties == null) {
            return null;
        }

        // 获取配置文件中的参数
        value = properties.getProperty(key);
        if (printLog) {
            logger.info("使用配置文件中的参数 [{}] {}", key, value);
        }
        return value;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param javaCG2OtherConfigFileUseSetEnum
     * @return
     */
    public Set<String> getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum javaCG2OtherConfigFileUseSetEnum, boolean printLog) {
        String configFileName = javaCG2OtherConfigFileUseSetEnum.getFileName();
        // 优先获取通过代码添加的参数
        Set<String> configSet = otherConfigSetMap.get(configFileName);
        if (configSet != null) {
            if (printLog) {
                logger.info("使用通过代码添加的参数 [{}]\n{}", configFileName, StringUtils.join(new ArrayList<>(configSet), " "));
            }
            return configSet;
        }

        // 获取其他配置文件中的参数
        configSet = JavaCG2FileUtil.readFile2Set(JavaCG2ConfManager.getInputRootPath() + configFileName);
        if (printLog) {
            logger.info("使用配置文件中的参数 [{}]\n{}", configFileName, StringUtils.join(new ArrayList<>(configSet), " "));
        }
        return configSet;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param javaCG2OtherConfigFileUseListEnum
     * @return
     */
    public List<String> getOtherConfigList(JavaCG2OtherConfigFileUseListEnum javaCG2OtherConfigFileUseListEnum, boolean printLog) {
        String configFileName = javaCG2OtherConfigFileUseListEnum.getFileName();
        // 优先获取通过代码添加的参数
        List<String> configList = otherConfigListMap.get(configFileName);
        if (configList != null) {
            if (printLog) {
                logger.info("使用通过代码添加的参数 [{}]\n{}", configFileName, StringUtils.join(configList, " "));
            }
            return configList;
        }

        // 获取其他配置文件中的参数
        configList = JavaCG2FileUtil.readFile2List(JavaCG2ConfManager.getInputRootPath() + configFileName);
        if (printLog) {
            logger.info("使用配置文件中的参数 [{}]\n{}", configFileName, StringUtils.join(configList, " "));
        }
        return configList;
    }
}
