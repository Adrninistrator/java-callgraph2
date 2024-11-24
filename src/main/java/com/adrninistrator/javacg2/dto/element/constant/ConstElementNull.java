package com.adrninistrator.javacg2.dto.element.constant;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description:
 */
public class ConstElementNull extends ConstElement {
    public ConstElementNull() {
        super(null);
    }

    @Override
    public JavaCG2ConstantTypeEnum getConstantTypeEnum() {
        return JavaCG2ConstantTypeEnum.CONSTTE_NULL;
    }

    @Override
    public BaseElement copyElement() {
        return new ConstElementNull();
    }
}
