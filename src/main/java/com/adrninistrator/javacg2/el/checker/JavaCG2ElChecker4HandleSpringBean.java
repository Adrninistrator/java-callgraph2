package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/10/18
 * @description:
 */
public class JavaCG2ElChecker4HandleSpringBean extends JavaCG2ElChecker {

    @Override
    protected void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig) {
        elManager.checkIgnoreSpringBeanInXml("bean1", JavaCG2ElChecker4ParseClass.CLASS_NAME_EXAMPLE, "dev");
    }
}
