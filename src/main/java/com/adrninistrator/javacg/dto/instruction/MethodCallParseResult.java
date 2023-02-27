package com.adrninistrator.javacg.dto.instruction;

import com.adrninistrator.javacg.dto.element.BaseElement;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/5/22
 * @description: 方法调用指令解析结果
 */
public class MethodCallParseResult extends BaseInstructionParseResult {

    // 方法调用对应的被调用对象
    private final BaseElement objectElement;

    // 方法调用对应的参数
    private final List<BaseElement> argumentList;

    public MethodCallParseResult(BaseElement objectElement, List<BaseElement> argumentList) {
        this.objectElement = objectElement;
        this.argumentList = argumentList;
    }

    public BaseElement getObjectElement() {
        return objectElement;
    }

    public List<BaseElement> getArgumentList() {
        return argumentList;
    }

    @Override
    public String toString() {
        return "MethodCallObjRefAndArgs{" +
                "\nobjectReference=" + objectElement +
                "\n, argumentList=" + argumentList +
                '}';
    }
}
