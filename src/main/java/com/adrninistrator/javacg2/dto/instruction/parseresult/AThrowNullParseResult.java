package com.adrninistrator.javacg2.dto.instruction.parseresult;

/**
 * @author adrninistrator
 * @date 2024/1/15
 * @description: ATHROW指令解析结果，抛出异常为null（throw null是合法的写法）
 */
public class AThrowNullParseResult extends AThrowParseResult {

    public AThrowNullParseResult() {
        super(null);
    }
}
