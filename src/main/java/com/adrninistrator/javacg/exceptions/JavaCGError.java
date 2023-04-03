package com.adrninistrator.javacg.exceptions;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class JavaCGError extends Error {
    public JavaCGError() {
    }

    public JavaCGError(String message) {
        super(message);
    }

    public JavaCGError(String message, Throwable cause) {
        super(message, cause);
    }

    public JavaCGError(Throwable cause) {
        super(cause);
    }

    public JavaCGError(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
