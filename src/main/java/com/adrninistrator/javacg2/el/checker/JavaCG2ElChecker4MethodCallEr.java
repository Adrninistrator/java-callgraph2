package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description:
 */
public class JavaCG2ElChecker4MethodCallEr extends JavaCG2ElChecker {

    @Override
    protected void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig) {
        elManager.checkIgnoreMethodCallByEr(JavaCG2CallTypeEnum.values()[0].getType(), JavaCG2ElChecker4ParseMethod.FULL_METHOD_EXAMPLE);
    }
}
