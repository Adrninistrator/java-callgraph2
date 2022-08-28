package com.adrninistrator.javacg.dto.counter;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description: 记录当前方法调用的ID
 */
public class CallIdCounter {

    private int currentCallId;

    public static CallIdCounter newInstance() {
        CallIdCounter callIdCounter = new CallIdCounter();
        callIdCounter.setCurrentCallId(0);
        return callIdCounter;
    }

    // 方法调用ID加1后返回
    public int addAndGet() {
        return ++currentCallId;
    }

    //
    public int getCurrentCallId() {
        return currentCallId;
    }

    public void setCurrentCallId(int currentCallId) {
        this.currentCallId = currentCallId;
    }
}
