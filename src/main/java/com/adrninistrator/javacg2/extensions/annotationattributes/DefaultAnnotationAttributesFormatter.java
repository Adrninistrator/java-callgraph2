package com.adrninistrator.javacg2.extensions.annotationattributes;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import org.apache.bcel.classfile.ElementValuePair;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description:
 */
public class DefaultAnnotationAttributesFormatter implements AnnotationAttributesFormatterInterface {

    @Override
    public String format(ElementValuePair elementValuePair) {
        return encodeAnnotationValue(elementValuePair.getValue().toString());
    }

    public static String encodeAnnotationValue(String value) {
        return value.replace('\r', JavaCG2Constants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN)
                .replace('\n', JavaCG2Constants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED);
    }

    public static String decodeAnnotationValue(String value) {
        return value.replace(JavaCG2Constants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN, '\r')
                .replace(JavaCG2Constants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED, '\n');
    }
}
