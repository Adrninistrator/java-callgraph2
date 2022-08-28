package com.adrninistrator.javacg.common;

import java.util.concurrent.Callable;

/**
 * @author adrninistrator
 * @date 2022/5/14
 * @description:
 */
public class ClassNameConstants {

    public static final String CLASS_NAME_RUNNABLE = Runnable.class.getName();
    public static final String CLASS_NAME_CALLABLE = Callable.class.getName();
    public static final String CLASS_NAME_THREAD = Thread.class.getName();
    public static final String CLASS_NAME_OBJECT = Object.class.getName();
    public static final String CLASS_NAME_STRING = String.class.getName();
    public static final String CLASS_NAME_CLASS = Class.class.getName();
    public static final String CLASS_NAME_NULL_POINTER_EXCEPTION = NullPointerException.class.getName();

    private ClassNameConstants() {
        throw new IllegalStateException("illegal");
    }
}
