package com.adrninistrator.javacg2.dto.counter;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description: 计数器
 */
public class JavaCG2Counter {
    private int count;

    public JavaCG2Counter() {
        this(0);
    }

    public JavaCG2Counter(int count) {
        this.count = count;
    }

    // 计数加1后返回
    public int addAndGet() {
        return ++count;
    }

    // 计数增加指定数量后返回
    public int addAndGet(int num) {
        return count += num;
    }

    // 计数减 1后返回
    public int minusAndGet() {
        return --count;
    }

    //
    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    @Override
    public String toString() {
        return "count=" + count;
    }
}
