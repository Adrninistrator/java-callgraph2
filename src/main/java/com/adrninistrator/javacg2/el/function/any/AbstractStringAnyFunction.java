package com.adrninistrator.javacg2.el.function.any;

import com.adrninistrator.javacg2.el.util.ElUtil;
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
public abstract class AbstractStringAnyFunction extends AbstractVariadicFunction {

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

        String arg0String = ElUtil.getArgStringValue(args[0], env);
        for (int i = 1; i < args.length; i++) {
            String argString = ElUtil.getArgStringValue(args[i], env);
            if (checkString(arg0String, argString)) {
                return AviatorBoolean.TRUE;
            }
        }
        return AviatorBoolean.FALSE;
    }
}
