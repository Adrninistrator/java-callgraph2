package com.adrninistrator.javacg2.el.function;

import com.adrninistrator.javacg2.el.util.JavaCG2ElUtil;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description:
 */
public abstract class AbstractStringFunctionTwoArgs extends AbstractFunction {

    protected abstract boolean checkString(String argSrc, String argDst);

    @Override
    public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        String target = JavaCG2ElUtil.getArgStringValue(arg1, env);
        String param = JavaCG2ElUtil.getArgStringValue(arg2, env);

        return checkString(target, param) ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
    }
}
