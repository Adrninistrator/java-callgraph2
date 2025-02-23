package com.adrninistrator.javacg2.el.util;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description:
 */
public class ElUtil {

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

    private ElUtil() {
        throw new IllegalStateException("illegal");
    }
}
