package com.adrninistrator.javacg2.dto.instruction.parseresult;

import org.apache.bcel.generic.InstructionHandle;

/**
 * @author adrninistrator
 * @date 2022/10/29
 * @description: RET指令的解析结果
 */
public class RetParseResult extends BaseInstructionParseResult {

    // 对应的jsr指令的下一条指令
    private final InstructionHandle jsrNextIh;

    public RetParseResult(InstructionHandle jsrNextIh) {
        this.jsrNextIh = jsrNextIh;
    }

    public InstructionHandle getJsrNextIh() {
        return jsrNextIh;
    }
}
