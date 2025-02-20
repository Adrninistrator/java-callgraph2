package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.manager.ElManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description: 表达式检查抽象父类
 */
public abstract class ElChecker {

    private static final Logger logger = LoggerFactory.getLogger(ElChecker.class);

    /**
     * 执行检查表达式
     *
     * @param elManager
     * @param elConfig
     */
    protected abstract void doCheck(ElManager elManager, ElConfigInterface elConfig);

    /**
     * 检查表达式
     *
     * @param elManager
     * @param elConfig
     * @return
     */
    public boolean check(ElManager elManager, ElConfigInterface elConfig) {
        try {
            doCheck(elManager, elConfig);
            return true;
        } catch (Exception e) {
            logger.error("执行表达式进行检查时失败，需要修改对应的表达式 {} ", elConfig.getKey(), e);
            return false;
        }
    }
}
