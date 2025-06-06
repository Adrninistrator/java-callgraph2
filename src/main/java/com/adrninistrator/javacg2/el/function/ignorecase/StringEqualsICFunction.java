package com.adrninistrator.javacg2.el.function.ignorecase;

import com.adrninistrator.javacg2.el.enums.ElStringFunctionTwoArgsEnum;
import com.adrninistrator.javacg2.el.function.AbstractStringFunctionTwoArgs;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description:
 */
public class StringEqualsICFunction extends AbstractStringFunctionTwoArgs {
    @Override
    protected boolean checkString(String argSrc, String argDst) {
        return false;
    }

    @Override
    public String getName() {
        return ElStringFunctionTwoArgsEnum.EQUALS_IGNORE_CASE.getName();
    }
}
