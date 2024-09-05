package com.adrninistrator.javacg2.dto.instruction;

/**
 * @author adrninistrator
 * @date 2023/8/4
 * @description: 方法调用指令位置及被调用方法
 */
public class InvokeInstructionPosAndCallee {

    // 方法调用序号
    private final int invokeInstructionPosition;

    // 被调用者类名
    private final String calleeClassName;

    // 被调用者方法名
    private final String calleeMethodName;

    public InvokeInstructionPosAndCallee(int invokeInstructionPosition, String calleeClassName, String calleeMethodName) {
        this.invokeInstructionPosition = invokeInstructionPosition;
        this.calleeClassName = calleeClassName;
        this.calleeMethodName = calleeMethodName;
    }

    public int getInvokeInstructionPosition() {
        return invokeInstructionPosition;
    }

    public String getCalleeClassName() {
        return calleeClassName;
    }

    public String getCalleeMethodName() {
        return calleeMethodName;
    }
}
