package com.adrninistrator.javacg.dto.instruction;

import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description: return类指令的解析结果
 */
public class ReturnParseResult extends BaseInstructionParseResult {

    // 返回信息
    private final BaseElement returnInfo;

    public ReturnParseResult(BaseElement returnInfo) {
        this.returnInfo = returnInfo;
    }

    public BaseElement getReturnInfo() {
        return returnInfo;
    }
}
