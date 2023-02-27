package com.adrninistrator.javacg.dto.element.constant;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.enums.ConstantTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量基类
 */
public abstract class ConstElement extends BaseElement {

    protected ConstElement(Object value) {
        if (value != null) {
            this.value = value;
        }
    }

    /**
     * 返回当前常量的类型
     *
     * @return
     */
    public abstract ConstantTypeEnum getConstantTypeEnum();

    @Override
    public String getType() {
        return getConstantTypeEnum().getType();
    }
}
