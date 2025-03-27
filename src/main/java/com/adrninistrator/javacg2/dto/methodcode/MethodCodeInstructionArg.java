package com.adrninistrator.javacg2.dto.methodcode;

/**
 * @author adrninistrator
 * @date 2025/3/21
 * @description: 方法代码指令，包含一个参数
 */
public class MethodCodeInstructionArg extends MethodCodeInstruction {

    // 参数
    private String arg;

    public String getArg() {
        return arg;
    }

    public void setArg(String arg) {
        this.arg = arg;
    }
}
