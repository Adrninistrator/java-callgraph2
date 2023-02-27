package com.adrninistrator.javacg.dto.element.constant;

import com.adrninistrator.javacg.enums.ConstantTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/5/14
 * @description:
 */
public class ConstElementShort extends ConstElement {

    public ConstElementShort(Object value) {
        super(value);
    }

    @Override
    public ConstantTypeEnum getConstantTypeEnum() {
        return ConstantTypeEnum.CONSTTE_SHORT;
    }
}
