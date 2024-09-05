package com.adrninistrator.javacg2.common;

import org.apache.bcel.generic.GETFIELD;
import org.apache.bcel.generic.INVOKESTATIC;

import java.util.TimerTask;
import java.util.concurrent.Callable;

/**
 * @author adrninistrator
 * @date 2023/2/10
 * @description:
 */
public class JavaCG2CommonNameConstants {
    public static final String CLASS_NAME_RUNNABLE = Runnable.class.getName();
    public static final String CLASS_NAME_CALLABLE = Callable.class.getName();
    public static final String CLASS_NAME_THREAD = Thread.class.getName();
    public static final String CLASS_NAME_TIMER_TASK = TimerTask.class.getName();
    public static final String CLASS_NAME_OBJECT = Object.class.getName();
    public static final String CLASS_NAME_STRING = String.class.getName();
    public static final String CLASS_NAME_CHAR_SEQUENCE = CharSequence.class.getName();
    public static final String CLASS_NAME_CLASS = Class.class.getName();
    public static final String CLASS_NAME_THROWABLE = Throwable.class.getName();
    public static final String CLASS_NAME_NULL_POINTER_EXCEPTION = NullPointerException.class.getName();

    public static final String CLASS_NAME_TRANSACTION_CALLBACK = "org.springframework.transaction.support.TransactionCallback";
    public static final String CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT = "org.springframework.transaction.support.TransactionCallbackWithoutResult";

    public static final String CLASS_NAME_GET_FIELD = GETFIELD.class.getName();

    public static final String SIMPLE_CLASS_NAME_INVOKE_STATIC = INVOKESTATIC.class.getSimpleName();
    public static final String SIMPLE_CLASS_NAME_OBJECT = Object.class.getSimpleName();

    public static final String PACKAGE_JAVA = "java.";

    public static final String METHOD_NAME_INIT = "<init>";
    public static final String METHOD_NAME_CLINIT = "<clinit>";
    public static final String METHOD_NAME_START = "start";
    public static final String METHOD_NAME_CLOSE = "close";
    public static final String METHOD_NAME_ADD_SUPPRESSED = "addSuppressed";

    public static final String METHOD_DO_IN_TRANSACTION = "doInTransaction";
    public static final String METHOD_DO_IN_TRANSACTION_WITHOUT_RESULT = "doInTransactionWithoutResult";

    public static final String ARGS_TRANSACTION_STATUS =
            JavaCG2Constants.FLAG_LEFT_BRACKET + "org.springframework.transaction.TransactionStatus" + JavaCG2Constants.FLAG_RIGHT_BRACKET;

    public static final String METHOD_RUNNABLE_RUN = "run";
    public static final String METHOD_CALLABLE_CALL = "call";

    public static final String RETURN_TYPE_VOID = "void";

    public static final String CATCH = "catch";
    public static final String FINALLY = "finally";

    public static final String SWITCH_MAP = "$SwitchMap$";

    public static final String MODIFIERS_PUBLIC = "public";
    public static final String MODIFIERS_PROTECTED = "protected";
    public static final String MODIFIERS_PRIVATE = "private";
    public static final String MODIFIERS_DEFAULT = "default";

    private JavaCG2CommonNameConstants() {
        throw new IllegalStateException("illegal");
    }
}
