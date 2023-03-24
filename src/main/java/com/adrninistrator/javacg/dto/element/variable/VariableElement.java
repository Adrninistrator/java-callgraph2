package com.adrninistrator.javacg.dto.element.variable;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 变量基类
 */
public class VariableElement extends BaseElement {

    public VariableElement(String type) {
        super(type, false);
    }

    public VariableElement(String type, boolean arrayElement) {
        super(type, arrayElement);
    }
}
