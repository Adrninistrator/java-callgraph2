package com.adrninistrator.javacg2.dto.instruction.parseresult;

import com.adrninistrator.javacg2.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2023/8/3
 * @description: PUTSTATIC指令解析结果
 */
public class PutStaticParseResult extends BaseInstructionParseResult {

    // 类名
    private final String className;

    // 字段名称
    private final String fieldName;

    // 字段类型
    private final String fieldType;

    // 对应的值
    private final BaseElement value;

    public PutStaticParseResult(String className, String fieldName, String fieldType, BaseElement value) {
        this.className = className;
        this.fieldName = fieldName;
        this.fieldType = fieldType;
        this.value = value;
    }

    public String getClassName() {
        return className;
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
}
