package com.adrninistrator.javacg.dto.instruction.parseresult;

import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2023/7/16
 * @description: PUTFIELD指令解析结果
 */
public class PutFieldParseResult extends BaseInstructionParseResult {

    // 字段名称
    private final String fieldName;

    // 字段类型
    private final String fieldType;

    // 对应的值
    private final BaseElement value;

    // 对应的对象
    private final BaseElement object;

    public PutFieldParseResult(String fieldName, String fieldType, BaseElement value, BaseElement object) {
        this.fieldName = fieldName;
        this.fieldType = fieldType;
        this.value = value;
        this.object = object;
    }

    public String getFieldName() {
        return fieldName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public BaseElement getValue() {
        return value;
    }

    public BaseElement getObject() {
        return object;
    }
}
