package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2022/11/2
 * @description:
 */
public class JavaCGElementUtil {
    /**
     * 获取元素类型
     *
     * @param baseElement
     * @return
     */
    public static String getElementType(BaseElement baseElement) {
        if (baseElement == null) {
            return null;
        }

        return baseElement.getType();
    }

    /**
     * 比较已存在的对象与另一个准备添加的对象值是否相同
     *
     * @param existed
     * @param added
     * @return false: 不相同 true: 相同
     */
    public static boolean compare(BaseElement existed, BaseElement added) {
        // 判断类型
        String existedType = existed.getType();
        String addedType = added.getType();

        if (!JavaCGByteCodeUtil.isNullType(addedType)) {
            if (JavaCGByteCodeUtil.isNullType(existedType)) {
                return false;
            } else if (!JavaCGByteCodeUtil.compareType(addedType, existedType)) {
                return false;
            }
        }

        // 判断值
        Object addedValue = added.getValue();
        if (addedValue != null) {
            Object existedValue = existed.getValue();
            if (existedValue == null) {
                return false;
            } else if (!addedValue.equals(existedValue)) {
                return false;
            }
        }

        if (existed instanceof FieldElement && added instanceof FieldElement) {
            // 对于字段（非静态或静态），还需要比较字段名称
            FieldElement existedFieldElement = (FieldElement) existed;
            FieldElement addedFieldElement = (FieldElement) added;
            return StringUtils.equals(existedFieldElement.getFieldName(), addedFieldElement.getFieldName());
        }

        return true;
    }

    private JavaCGElementUtil() {
        throw new IllegalStateException("illegal");
    }
}
