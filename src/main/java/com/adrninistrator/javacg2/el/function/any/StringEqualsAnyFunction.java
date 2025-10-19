package com.adrninistrator.javacg2.el.function.any;

import com.adrninistrator.javacg2.el.enums.ElStringAnyFunctionEnum;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2025/2/8
 * @description:
 */
public class StringEqualsAnyFunction extends AbstractStringAnyFunction {

    @Override
    public String getName() {
        return ElStringAnyFunctionEnum.EQUALS_ANY.getName();
    }

    @Override
    protected boolean checkString(String arg0, String argN) {
        return StringUtils.equals(arg0, argN);
    }
}
