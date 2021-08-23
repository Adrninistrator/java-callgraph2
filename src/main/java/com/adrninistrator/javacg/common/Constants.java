package com.adrninistrator.javacg.common;

/**
 * @author adrninistrator
 * @date 2021/6/26
 * @description:
 */

public class Constants {

    public static final String FLAG_LAMBDA = "lambda$";

    public static final int FLAG_LAMBDA_LENGTH = FLAG_LAMBDA.length();

    public static final int DEFAULT_LINE_NUMBER = 0;
    public static final int NONE_LINE_NUMBER = -1;

    public static final String METHOD_NAME_INIT = "<init>";

    public static final String METHOD_NAME_START = "start";

    public static final String NEW_LINE = "\n";

    private Constants() {
        throw new IllegalStateException("illegal");
    }
}
