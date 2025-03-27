package com.adrninistrator.javacg2.dto.methodcode;

/**
 * @author adrninistrator
 * @date 2025/3/21
 * @description: lookupswitch指令中的数据
 */
public class MethodCodeInstructionLookupSwitchJump {

    // 索引
    private int index;

    // 跳转目标指令偏移量
    private int targetOffset;

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public int getTargetOffset() {
        return targetOffset;
    }

    public void setTargetOffset(int targetOffset) {
        this.targetOffset = targetOffset;
    }
}
