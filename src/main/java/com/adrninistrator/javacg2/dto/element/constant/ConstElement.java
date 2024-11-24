package com.adrninistrator.javacg2.dto.element.constant;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量基类
 */
public abstract class ConstElement extends BaseElement {

    ConstElement(Object value) {
        if (value != null) {
            this.value = value;
        }
    }

    /**
     * 返回当前常量的类型
     *
     * @return
     */
    public JavaCG2ConstantTypeEnum getConstantTypeEnum() {
        return null;
    }

    @Override
    public String getType() {
        JavaCG2ConstantTypeEnum javaCG2ConstantTypeEnum = getConstantTypeEnum();
        if (javaCG2ConstantTypeEnum == null) {
            return null;
        }
        return javaCG2ConstantTypeEnum.getType();
    }
}
