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

        if (!JavaCGByteCodeUtil.isNullType(addedType) &&
                (JavaCGByteCodeUtil.isNullType(existedType) || !JavaCGByteCodeUtil.compareType(addedType, existedType))) {
            /*
                1. 假如需要添加的类型不是null则继续判断是否与已存在的类型不同（若需要添加的类型为null则不通过类型判断是否不同，继续判断后续条件）
                2. 假如已存在的类型是null，或者已存在的类型与需要添加的类型不同时，返回不相同
             */
            return false;
        }

        // 判断值
        Object addedValue = added.getValue();
        if (addedValue != null && !addedValue.equals(existed.getValue())) {
            /*
                假如需要添加的值非空，且与已存在的值不同时，返回不相同
                （假如需要添加的值为空，则不通过值判断是否不同，继续判断后续条件）
             */
            return false;
        }

        if (existed instanceof FieldElement && added instanceof FieldElement) {
            // 对于字段（非静态或静态），还需要比较字段名称
            FieldElement existedFieldElement = (FieldElement) existed;
            FieldElement addedFieldElement = (FieldElement) added;
            return StringUtils.equals(existedFieldElement.getFieldName(), addedFieldElement.getFieldName());
        }
        // 对于需要添加的对象和已存在的对象都属于数组，且确实不相同的情况，不进行处理，意义不大
        return true;
    }

    private JavaCGElementUtil() {
        throw new IllegalStateException("illegal");
    }
}
