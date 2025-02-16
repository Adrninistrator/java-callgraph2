package com.adrninistrator.javacg2.el.function;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;
import org.apache.commons.lang3.ArrayUtils;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/10
 * @description: 类似 StringUtils.xxxAny 功能的自定义函数基类
 */
public abstract class StringAnyFunction extends AbstractVariadicFunction {

    /**
     * 比较字符串
     *
     * @param arg0 第1个参数字符串值
     * @param argN 第n（n>1）个参数字符串值
     * @return
     */
    protected abstract boolean checkString(String arg0, String argN);

    @Override
    public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {
        if (ArrayUtils.isEmpty(args)) {
            throw new ExpressionRuntimeException("参数不允许为空");
        }

        if (args.length < 2) {
            throw new ExpressionRuntimeException("至少需要有两个参数");
        }

        String arg0String = getArgStringValue(env, args[0]);
        for (int i = 1; i < args.length; i++) {
            String argString = getArgStringValue(env, args[i]);
            if (checkString(arg0String, argString)) {
                return AviatorBoolean.TRUE;
            }
        }
        return AviatorBoolean.FALSE;
    }

    /**
     * 获取参数的字符串类型的值
     *
     * @param env
     * @param arg
     * @return
     */
    protected String getArgStringValue(Map<String, Object> env, AviatorObject arg) {
        Object argValue = arg.getValue(env);
        if (!(argValue instanceof String)) {
            throw new ExpressionRuntimeException("只允许使用字符串类型参数");
        }
        return (String) argValue;
    }
}
