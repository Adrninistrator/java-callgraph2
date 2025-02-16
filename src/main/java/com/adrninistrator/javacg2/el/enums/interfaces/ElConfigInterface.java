package com.adrninistrator.javacg2.el.enums.interfaces;

import com.adrninistrator.javacg2.conf.enums.interfaces.ConfigInterface;
import com.adrninistrator.javacg2.el.checker.ElChecker;

/**
 * @author adrninistrator
 * @date 2025/2/1
 * @description: 表达式语言配置接口
 */
public interface ElConfigInterface extends ConfigInterface {

    // 获取配置文件允许使用的表达式语言变量枚举
    ElAllowedVariableInterface[] getElAllowedVariableEnums();

    // 用于提前执行表达式进行检查的类
    Class<? extends ElChecker> getElCheckClass();
}
