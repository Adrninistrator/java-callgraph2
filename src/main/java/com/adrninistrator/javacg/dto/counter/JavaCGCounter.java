package com.adrninistrator.javacg.dto.counter;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description: 计数器
 */
public class JavaCGCounter {
    private int count;

    public JavaCGCounter(int count) {
        this.count = count;
    }

    // 计数加1后返回
    public int addAndGet() {
        return ++count;
    }

    //
    public int getCount() {
        return count;
    }
}
