package com.adrninistrator.javacg.dto.element.constant;

import com.adrninistrator.javacg.enums.ConstantTypeEnum;

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
    public ConstantTypeEnum getConstantTypeEnum() {
        return ConstantTypeEnum.CONSTTE_NULL;
    }
}
