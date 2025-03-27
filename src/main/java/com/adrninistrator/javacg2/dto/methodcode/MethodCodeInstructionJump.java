package com.adrninistrator.javacg2.dto.methodcode;

/**
 * @author adrninistrator
 * @date 2025/3/21
 * @description: 方法代码指令，跳转
 */
public class MethodCodeInstructionJump extends MethodCodeInstruction {

    // 跳转目标指令偏移量
    private int targetOffset;

    public int getTargetOffset() {
        return targetOffset;
    }

    public void setTargetOffset(int targetOffset) {
        this.targetOffset = targetOffset;
    }
}
