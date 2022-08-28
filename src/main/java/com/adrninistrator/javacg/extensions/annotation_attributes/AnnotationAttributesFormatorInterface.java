package com.adrninistrator.javacg.extensions.annotation_attributes;

import org.apache.bcel.classfile.ElementValuePair;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对注解属性的元素值进行格式化的接口
 */
public interface AnnotationAttributesFormatorInterface {

    String format(ElementValuePair elementValuePair);
}
