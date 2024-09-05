package com.adrninistrator.javacg2.exceptions;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class JavaCG2Error extends Error {
    public JavaCG2Error() {
    }

    public JavaCG2Error(String message) {
        super(message);
    }

    public JavaCG2Error(String message, Throwable cause) {
        super(message, cause);
    }

    public JavaCG2Error(Throwable cause) {
        super(cause);
    }

    public JavaCG2Error(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
