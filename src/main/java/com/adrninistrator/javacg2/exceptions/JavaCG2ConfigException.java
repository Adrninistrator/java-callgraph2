package com.adrninistrator.javacg2.exceptions;

/**
 * @author adrninistrator
 * @date 2026/5/24
 * @description: 配置参数异常
 */
public class JavaCG2ConfigException extends RuntimeException {

    public JavaCG2ConfigException() {
    }

    public JavaCG2ConfigException(String message) {
        super(message);
    }

    public JavaCG2ConfigException(String message, Throwable cause) {
        super(message, cause);
    }

    public JavaCG2ConfigException(Throwable cause) {
        super(cause);
    }

    public JavaCG2ConfigException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
