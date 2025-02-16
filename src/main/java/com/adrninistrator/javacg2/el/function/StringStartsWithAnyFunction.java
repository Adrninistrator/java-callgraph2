package com.adrninistrator.javacg2.el.function;

import com.adrninistrator.javacg2.el.enums.StringAnyFunctionEnum;

/**
 * @author adrninistrator
 * @date 2025/2/8
 * @description:
 */
public class StringStartsWithAnyFunction extends StringAnyFunction {

    @Override
    public String getName() {
        return StringAnyFunctionEnum.STARTS_WITH_ANY.getName();
    }

    @Override
    protected boolean checkString(String arg0, String argN) {
        return arg0.startsWith(argN);
    }
}
