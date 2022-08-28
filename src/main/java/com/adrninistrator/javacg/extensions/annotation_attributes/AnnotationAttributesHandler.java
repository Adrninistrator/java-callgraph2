package com.adrninistrator.javacg.extensions.annotation_attributes;

import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.ElementValuePair;
import org.apache.bcel.classfile.Utility;

import java.io.Writer;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对注解属性值进行处理的类
 */
public class AnnotationAttributesHandler {

    /**
     * 将注解信息写入文件
     *
     * @param type              当前注解对应的元素类型，类还是方法
     * @param classOrMethod     类名或方法
     * @param annotationEntries
     * @param writer
     */
    public static void writeAnnotationInfo(String type,
                                           String classOrMethod,
                                           AnnotationEntry[] annotationEntries,
                                           AnnotationAttributesFormatorInterface annotationAttributesFormator,
                                           Writer writer) {
        if (annotationEntries == null || annotationEntries.length == 0) {
            return;
        }

        try {
            StringBuilder stringBuilder = new StringBuilder();
            for (AnnotationEntry annotationEntry : annotationEntries) {
                String annotationClassName = Utility.typeSignatureToString(annotationEntry.getAnnotationType(), false);
                // TODO 后面把空格改成\t
                String data = type + " " + classOrMethod + " " + annotationClassName;

                if (annotationEntry.getElementValuePairs() == null || annotationEntry.getElementValuePairs().length == 0) {
                    // 注解属性为空
                    stringBuilder.append(data).append(JavaCGConstants.NEW_LINE);
                    continue;
                }

                // 注解属性非空
                for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
                    String formattedValue = annotationAttributesFormator.format(elementValuePair);

                    stringBuilder.append(data).append(" ").append(formattedValue).append(JavaCGConstants.NEW_LINE);
                }
            }

            writer.write(stringBuilder.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private AnnotationAttributesHandler() {
        throw new IllegalStateException("illegal");
    }
}
