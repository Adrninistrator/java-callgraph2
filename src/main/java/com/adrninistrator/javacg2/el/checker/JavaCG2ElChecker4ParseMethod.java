package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description:
 */
public class JavaCG2ElChecker4ParseMethod extends JavaCG2ElChecker {

    public static final String FULL_METHOD_EXAMPLE = JavaCG2ElChecker4ParseClass.CLASS_NAME_EXAMPLE + ":fun(int)";

    @Override
    protected void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig) {
        elManager.checkIgnoreParseMethod(FULL_METHOD_EXAMPLE);
    }
}
