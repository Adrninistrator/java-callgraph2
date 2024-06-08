package com.adrninistrator.javacg.dto.instruction.parseresult;

import com.adrninistrator.javacg.dto.element.variable.VariableElement;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description: ATHROW指令解析结果
 */
public class AThrowParseResult extends BaseInstructionParseResult {

    // 抛出的异常
    private final VariableElement throwElement;

    public AThrowParseResult(VariableElement throwElement) {
        this.throwElement = throwElement;
    }

    public VariableElement getThrowElement() {
        return throwElement;
    }
}
