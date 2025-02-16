package com.adrninistrator.javacg2.el.function;

import com.adrninistrator.javacg2.el.enums.StringAnyFunctionEnum;

/**
 * @author adrninistrator
 * @date 2025/2/8
 * @description:
 */
public class StringEqualsAnyFunction extends StringAnyFunction {

    @Override
    public String getName() {
        return StringAnyFunctionEnum.EQUALS_ANY.getName();
    }

    @Override
    protected boolean checkString(String arg0, String argN) {
        return arg0.equals(argN);
    }
}
