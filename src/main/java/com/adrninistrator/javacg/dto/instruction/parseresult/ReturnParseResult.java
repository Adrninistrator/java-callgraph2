package com.adrninistrator.javacg.dto.instruction.parseresult;

import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description: RETURN类指令的解析结果
 */
public class ReturnParseResult extends BaseInstructionParseResult {

    // 返回元素
    private final BaseElement returnElement;

    public ReturnParseResult(BaseElement returnElement) {
        this.returnElement = returnElement;
    }

    public BaseElement getReturnElement() {
        return returnElement;
    }
}
