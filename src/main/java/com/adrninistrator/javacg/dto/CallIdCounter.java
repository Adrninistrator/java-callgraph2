package com.adrninistrator.javacg.dto;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description: 记录当前方法启用的ID
 */
public class CallIdCounter {

    private int currentCallId;

    public static CallIdCounter newInstance() {
        CallIdCounter callIdCounter = new CallIdCounter();
        callIdCounter.setCurrentCallId(0);
        return callIdCounter;
    }

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
