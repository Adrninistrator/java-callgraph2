package com.adrninistrator.javacg2.exceptions;

import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;

/**
 * @author adrninistrator
 * @date 2026/3/28
 * @description: 表达式配置相关运行时异常，包含出错的ElConfigInterface对象
 */
public class JavaCG2ElConfigRuntimeException extends JavaCG2RuntimeException {

    private final ElConfigInterface elConfig;

    public JavaCG2ElConfigRuntimeException(String message, ElConfigInterface elConfig) {
        super(message);
        this.elConfig = elConfig;
    }

    public JavaCG2ElConfigRuntimeException(String message, Throwable cause, ElConfigInterface elConfig) {
        super(message, cause);
        this.elConfig = elConfig;
    }

    /**
     * 获取出错的ElConfigInterface对象
     *
     * @return ElConfigInterface对象
     */
    public ElConfigInterface getElConfig() {
        return elConfig;
    }
}
