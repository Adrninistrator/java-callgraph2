package com.adrninistrator.javacg2.dto.element.constant;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;

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
    public JavaCG2ConstantTypeEnum getConstantTypeEnum() {
        return JavaCG2ConstantTypeEnum.CONSTTE_SHORT;
    }
}
