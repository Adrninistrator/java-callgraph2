package com.adrninistrator.javacg.dto.element.constant;

import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description:
 */
public class ConstElementInt extends ConstElement {

    public ConstElementInt(Object value) {
        super(value);
    }

    @Override
    public JavaCGConstantTypeEnum getConstantTypeEnum() {
        return JavaCGConstantTypeEnum.CONSTTE_INT;
    }
}
