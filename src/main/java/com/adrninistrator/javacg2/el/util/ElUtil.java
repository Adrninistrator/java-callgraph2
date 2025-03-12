package com.adrninistrator.javacg2.el.util;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description: 表达式工具类
 */
public class ElUtil {

    // 判断是否是执行用于检测的表达式
    private static final ThreadLocal<Boolean> RUN_IN_CHECKER_FLAG = new ThreadLocal<>();

    /**
     * 获取参数的字符串类型的值
     *
     * @param arg
     * @param env
     * @return
     */
    public static String getArgStringValue(AviatorObject arg, Map<String, Object> env) {
        Object argValue = arg.getValue(env);
        if (!(argValue instanceof String)) {
            throw new ExpressionRuntimeException("只允许使用字符串类型参数");
        }
        return (String) argValue;
    }

    // 设置执行用于检测的表达式标志
    public static void setRunInCheckerFlag() {
        RUN_IN_CHECKER_FLAG.set(Boolean.TRUE);
    }

    // 清理执行用于检测的表达式标志
    public static void clearRunInCheckerFlag() {
        RUN_IN_CHECKER_FLAG.remove();
    }

    // 检查执行用于检测的表达式标志
    public static boolean checkRunInCheckerFlag() {
        return Boolean.TRUE.equals(RUN_IN_CHECKER_FLAG.get());
    }

    private ElUtil() {
        throw new IllegalStateException("illegal");
    }
}
