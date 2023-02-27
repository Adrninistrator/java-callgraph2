package com.adrninistrator.javacg.common;

import java.util.TimerTask;
import java.util.concurrent.Callable;

/**
 * @author adrninistrator
 * @date 2023/2/10
 * @description:
 */
public class JavaCGCommonNameConstants {
    public static final String CLASS_NAME_RUNNABLE = Runnable.class.getName();
    public static final String CLASS_NAME_CALLABLE = Callable.class.getName();
    public static final String CLASS_NAME_THREAD = Thread.class.getName();
    public static final String CLASS_NAME_TIMER_TASK = TimerTask.class.getName();
    public static final String CLASS_NAME_OBJECT = Object.class.getName();
    public static final String CLASS_NAME_STRING = String.class.getName();
    public static final String CLASS_NAME_CLASS = Class.class.getName();
    public static final String CLASS_NAME_NULL_POINTER_EXCEPTION = NullPointerException.class.getName();

    public static final String CLASS_NAME_TRANSACTION_CALLBACK = "org.springframework.transaction.support.TransactionCallback";
    public static final String CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT = "org.springframework.transaction.support.TransactionCallbackWithoutResult";

    public static final String SIMPLE_CLASS_NAME_OBJECT = Object.class.getSimpleName();

    public static final String METHOD_DO_IN_TRANSACTION="doInTransaction";
    public static final String METHOD_DO_IN_TRANSACTION_WITHOUT_RESULT="doInTransactionWithoutResult";

    public static final String ARGS_TRANSACTION_STATUS =
            JavaCGConstants.FLAG_LEFT_BRACKET + "org.springframework.transaction.TransactionStatus" + JavaCGConstants.FLAG_RIGHT_BRACKET;

    private JavaCGCommonNameConstants() {
        throw new IllegalStateException("illegal");
    }
}
