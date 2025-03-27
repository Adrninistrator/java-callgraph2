package com.adrninistrator.javacg2.dto.methodcode;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/3/20
 * @description: 方法代码指令，lookupswitch
 */
public class MethodCodeInstructionLookupSwitch extends MethodCodeInstruction {

    // 默认跳转目标指令偏移量
    private int defaultTargetOffset;

    // 键值对数量
    private int pairsNum;

    // 跳转目标列表
    private List<MethodCodeInstructionLookupSwitchJump> jumpList;

    public int getDefaultTargetOffset() {
        return defaultTargetOffset;
    }

    public void setDefaultTargetOffset(int defaultTargetOffset) {
        this.defaultTargetOffset = defaultTargetOffset;
    }

    public int getPairsNum() {
        return pairsNum;
    }

    public void setPairsNum(int pairsNum) {
        this.pairsNum = pairsNum;
    }

    public List<MethodCodeInstructionLookupSwitchJump> getJumpList() {
        return jumpList;
    }

    public void setJumpList(List<MethodCodeInstructionLookupSwitchJump> jumpList) {
        this.jumpList = jumpList;
    }
}
