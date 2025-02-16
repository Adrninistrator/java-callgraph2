package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description:
 */
public class JavaCG2ElChecker4MethodCallErEe extends JavaCG2ElChecker {

    @Override
    protected void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig) {
        elManager.checkIgnoreMethodCallByErEe(JavaCG2ElChecker4ParseMethod.FULL_METHOD_EXAMPLE, JavaCG2ElChecker4ParseMethod.FULL_METHOD_EXAMPLE);
    }
}
