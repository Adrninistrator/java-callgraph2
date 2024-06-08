package com.adrninistrator.javacg.dto.element.constant;

import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 常量基类
 */
public class ConstElement extends BaseElement {

    ConstElement(Object value) {
        if (value != null) {
            this.value = value;
        }
    }

    @Override
    public BaseElement copyElement() {
        return new ConstElement(value);
    }

    /**
     * 返回当前常量的类型
     *
     * @return
     */
    public JavaCGConstantTypeEnum getConstantTypeEnum() {
        return null;
    }

    @Override
    public String getType() {
        JavaCGConstantTypeEnum javaCGConstantTypeEnum = getConstantTypeEnum();
        if (javaCGConstantTypeEnum == null) {
            return null;
        }
        return javaCGConstantTypeEnum.getType();
    }
}
