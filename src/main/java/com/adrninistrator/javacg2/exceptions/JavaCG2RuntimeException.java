package com.adrninistrator.javacg2.exceptions;

/**
 * @author adrninistrator
 * @date 2023/2/13
 * @description:
 */
public class JavaCG2RuntimeException extends RuntimeException {
    public JavaCG2RuntimeException() {
    }

    public JavaCG2RuntimeException(String message) {
        super(message);
    }

    public JavaCG2RuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

    public JavaCG2RuntimeException(Throwable cause) {
        super(cause);
    }

    public JavaCG2RuntimeException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
