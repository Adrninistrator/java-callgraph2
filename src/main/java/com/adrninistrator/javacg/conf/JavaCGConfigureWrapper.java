package com.adrninistrator.javacg.conf;

import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.exceptions.JavaCGError;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description: 配置参数包装类
 */
public class JavaCGConfigureWrapper {
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
    public JavaCGConfigureWrapper() {
        this(true);
    }

    /**
     * @param useDefaultEmptyConfigFlag true: 使用默认的空参数（忽略配置文件中的参数） false: 使用配置文件中的参数
     */
    public JavaCGConfigureWrapper(boolean useDefaultEmptyConfigFlag) {
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
        for (JavaCGConfigKeyEnum javaCGConfigKeyEnum : JavaCGConfigKeyEnum.values()) {
            configMap.put(javaCGConfigKeyEnum.getKey(), "");
        }
        for (JavaCGOtherConfigFileUseSetEnum javaCGOtherConfigFileUseSetEnum : JavaCGOtherConfigFileUseSetEnum.values()) {
            otherConfigSetMap.put(javaCGOtherConfigFileUseSetEnum.getFileName(), Collections.emptySet());
        }
        for (JavaCGOtherConfigFileUseListEnum javaCGOtherConfigFileUseListEnum : JavaCGOtherConfigFileUseListEnum.values()) {
            otherConfigListMap.put(javaCGOtherConfigFileUseListEnum.getFileName(), Collections.emptyList());
        }
    }

    /**
     * 设置配置文件中的参数
     *
     * @param javaCGConfigKeyEnum
     * @param value
     */
    public void setConfig(JavaCGConfigKeyEnum javaCGConfigKeyEnum, String value) {
        if (value == null) {
            return;
        }

        configMap.put(javaCGConfigKeyEnum.getKey(), value);
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCGOtherConfigFileUseSetEnum
     * @param data
     */
    public void setOtherConfigSet(JavaCGOtherConfigFileUseSetEnum javaCGOtherConfigFileUseSetEnum, String... data) {
        setOtherConfigSet(javaCGOtherConfigFileUseSetEnum, JavaCGUtil.genSetFromArray(data));
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCGOtherConfigFileUseSetEnum
     * @param configSet
     */
    public void setOtherConfigSet(JavaCGOtherConfigFileUseSetEnum javaCGOtherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            throw new JavaCGError("不允许传入null，只能传入内容为空的Set");
        }
        otherConfigSetMap.put(javaCGOtherConfigFileUseSetEnum.getFileName(), configSet);
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCGOtherConfigFileUseListEnum
     * @param data
     */
    public void setOtherConfigList(JavaCGOtherConfigFileUseListEnum javaCGOtherConfigFileUseListEnum, String... data) {
        setOtherConfigList(javaCGOtherConfigFileUseListEnum, JavaCGUtil.genListFromArray(data));
    }

    /**
     * 设置其他配置文件中的参数
     *
     * @param javaCGOtherConfigFileUseListEnum
     * @param configList
     */
    public void setOtherConfigList(JavaCGOtherConfigFileUseListEnum javaCGOtherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            throw new JavaCGError("不允许传入null，只能传入内容为空的List");
        }
        otherConfigListMap.put(javaCGOtherConfigFileUseListEnum.getFileName(), configList);
    }

    /**
     * 获取配置文件中的参数，或通过代码添加的参数
     *
     * @param properties
     * @param javaCGConfigKeyEnum
     * @return
     */
    public String getConfig(Properties properties, JavaCGConfigKeyEnum javaCGConfigKeyEnum, boolean printLog) {
        String key = javaCGConfigKeyEnum.getKey();
        // 优先获取通过代码添加的参数
        String value = configMap.get(key);
        if (value != null) {
            if (printLog) {
                System.out.println("使用通过代码添加的参数 [" + key + "] " + value);
            }
            return value;
        }

        if (properties == null) {
            return null;
        }

        // 获取配置文件中的参数
        value = properties.getProperty(key);
        if (printLog) {
            System.out.println("使用配置文件中的参数 [" + key + "] " + value);
        }
        return value;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param javaCGOtherConfigFileUseSetEnum
     * @return
     */
    public Set<String> getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum javaCGOtherConfigFileUseSetEnum, boolean printLog) {
        String configFileName = javaCGOtherConfigFileUseSetEnum.getFileName();
        // 优先获取通过代码添加的参数
        Set<String> configSet = otherConfigSetMap.get(configFileName);
        if (configSet != null) {
            if (printLog) {
                System.out.println("使用通过代码添加的参数 [" + configFileName + "]\n" + StringUtils.join(new ArrayList<>(configSet), " "));
            }
            return configSet;
        }

        // 获取其他配置文件中的参数
        configSet = JavaCGFileUtil.readFile2Set(JavaCGConfManager.getInputRootPath() + configFileName);
        if (printLog) {
            System.out.println("使用配置文件中的参数 [" + configFileName + "]\n" + StringUtils.join(new ArrayList<>(configSet), " "));
        }
        return configSet;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param javaCGOtherConfigFileUseListEnum
     * @return
     */
    public List<String> getOtherConfigList(JavaCGOtherConfigFileUseListEnum javaCGOtherConfigFileUseListEnum, boolean printLog) {
        String configFileName = javaCGOtherConfigFileUseListEnum.getFileName();
        // 优先获取通过代码添加的参数
        List<String> configList = otherConfigListMap.get(configFileName);
        if (configList != null) {
            if (printLog) {
                System.out.println("使用通过代码添加的参数 [" + configFileName + "]\n" + StringUtils.join(configList, " "));
            }
            return configList;
        }

        // 获取其他配置文件中的参数
        configList = JavaCGFileUtil.readFile2List(JavaCGConfManager.getInputRootPath() + configFileName);
        if (printLog) {
            System.out.println("使用配置文件中的参数 [" + configFileName + "]\n" + StringUtils.join(configList, " "));
        }
        return configList;
    }
}
