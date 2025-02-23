package com.adrninistrator.javacg2.el.function.any;

import com.adrninistrator.javacg2.el.enums.ElStringAnyFunctionEnum;

/**
 * @author adrninistrator
 * @date 2025/2/8
 * @description:
 */
public class StringEndsWithAnyFunction extends AbstractStringAnyFunction {

    @Override
    public String getName() {
        return ElStringAnyFunctionEnum.ENDS_WITH_ANY.getName();
    }

    @Override
    protected boolean checkString(String arg0, String argN) {
        return arg0.endsWith(argN);
    }
}
