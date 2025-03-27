package com.adrninistrator.javacg2.dto.methodcode;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/3/21
 * @description: 方法代码指令，tableswitch
 */
public class MethodCodeInstructionTableSwitch extends MethodCodeInstruction {

    // 默认跳转目标指令偏移量
    private int defaultTargetOffset;

    // case最小值
    private int low;

    // case最大值
    private int high;

    // 跳转目标列表
    private List<Integer> jumpTargetOffsetList;

    public int getDefaultTargetOffset() {
        return defaultTargetOffset;
    }

    public void setDefaultTargetOffset(int defaultTargetOffset) {
        this.defaultTargetOffset = defaultTargetOffset;
    }

    public int getLow() {
        return low;
    }

    public void setLow(int low) {
        this.low = low;
    }

    public int getHigh() {
        return high;
    }

    public void setHigh(int high) {
        this.high = high;
    }

    public List<Integer> getJumpTargetOffsetList() {
        return jumpTargetOffsetList;
    }

    public void setJumpTargetOffsetList(List<Integer> jumpTargetOffsetList) {
        this.jumpTargetOffsetList = jumpTargetOffsetList;
    }
}
