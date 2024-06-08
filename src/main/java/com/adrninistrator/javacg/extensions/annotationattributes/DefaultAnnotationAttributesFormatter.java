package com.adrninistrator.javacg.extensions.annotationattributes;

import com.adrninistrator.javacg.common.JavaCGConstants;
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
        return value.replace('\r', JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN)
                .replace('\n', JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED);
    }

    public static String decodeAnnotationValue(String value) {
        return value.replace(JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN, '\r')
                .replace(JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED, '\n');
    }
}
