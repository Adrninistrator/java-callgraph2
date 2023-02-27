package com.adrninistrator.javacg.dto.branch;

import org.apache.bcel.generic.InstructionHandle;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/10/16
 * @description: 对分支指令进行遍历时使用的栈结构
 */
public class BranchStackEntry {

    // 分支指令
    private final InstructionHandle branchIh;

    // 目标指令列表
    private final List<InstructionHandle> targetIhList;

    // 当前处理的目标指令序号
    private int targetIhIndex;

    public BranchStackEntry(InstructionHandle branchIh, int targetNum) {
        this.branchIh = branchIh;
        targetIhList = new ArrayList<>(targetNum);
        targetIhIndex = 0;
    }

    public void addTargetIh(InstructionHandle targetIh) {
        targetIhList.add(targetIh);
    }

    //
    public InstructionHandle getBranchIh() {
        return branchIh;
    }

    public List<InstructionHandle> getTargetIhList() {
        return targetIhList;
    }

    public int getTargetIhIndex() {
        return targetIhIndex;
    }

    public void setTargetIhIndex(int targetIhIndex) {
        this.targetIhIndex = targetIhIndex;
    }

    @Override
    public String toString() {
        return "BranchStackEntry{" +
                "branchIh=" + branchIh +
                ", targetIhList=" + targetIhList +
                ", targetIhIndex=" + targetIhIndex +
                '}';
    }
}
