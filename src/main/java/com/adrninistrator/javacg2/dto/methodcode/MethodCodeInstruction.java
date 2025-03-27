package com.adrninistrator.javacg2.dto.methodcode;

/**
 * @author adrninistrator
 * @date 2025/3/18
 * @description: 方法代码指令
 */
public class MethodCodeInstruction {

    // 指令偏移量
    private int offset;

    // 指令名称
    private String instructionName;

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public String getInstructionName() {
        return instructionName;
    }

    public void setInstructionName(String instructionName) {
        this.instructionName = instructionName;
    }
}
