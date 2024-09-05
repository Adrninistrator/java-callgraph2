package com.adrninistrator.javacg2.extensions.codeparser;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description: 对jar包中的文件或字节码解析类的基础接口
 */
public interface CodeParserInterface {

    /**
     * 初始化，整个执行过程中只执行一次
     */
    default void initCodeParser() {
    }
}
