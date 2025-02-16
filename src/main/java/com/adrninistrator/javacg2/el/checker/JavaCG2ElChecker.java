package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.manager.ElManager;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description: 当前项目使用的表达式语言检查抽象父类
 */
public abstract class JavaCG2ElChecker extends ElChecker {

    @Override
    protected void doCheck(ElManager elManager, ElConfigInterface elConfig) {
        javaCG2DoCheck((JavaCG2ElManager) elManager, (JavaCG2ElConfigEnum) elConfig);
    }


    /**
     * 执行检查表达式语言
     *
     * @param elManager
     * @param elConfig
     */
    protected abstract void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig);
}
