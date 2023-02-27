package com.adrninistrator.javacg.dto.element.constant;

import com.adrninistrator.javacg.enums.ConstantTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description:
 */
public class ConstElementFloat extends ConstElement {

    public ConstElementFloat(Object value) {
        super(value);
    }

    @Override
    public ConstantTypeEnum getConstantTypeEnum() {
        return ConstantTypeEnum.CONSTTE_FLOAT;
    }
}
