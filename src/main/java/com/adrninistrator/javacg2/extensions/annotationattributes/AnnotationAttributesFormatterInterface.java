package com.adrninistrator.javacg2.extensions.annotationattributes;

import org.apache.bcel.classfile.ElementValuePair;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对注解属性的元素值进行格式化的接口
 */
public interface AnnotationAttributesFormatterInterface {

    String format(ElementValuePair elementValuePair);
}
