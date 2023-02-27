package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatterInterface;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.ArrayElementValue;
import org.apache.bcel.classfile.ClassElementValue;
import org.apache.bcel.classfile.ElementValue;
import org.apache.bcel.classfile.ElementValuePair;
import org.apache.bcel.classfile.SimpleElementValue;
import org.apache.bcel.classfile.Utility;
import org.apache.commons.lang3.StringUtils;

import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 对注解进行处理的工具类
 */
public class JavaCGAnnotationUtil {

    /**
     * 获取注解中指定名称的注解属性值，String类型
     *
     * @param annotationEntry
     * @param annotationAttributeNames
     * @return
     */
    public static String getAnnotationAttributeStringValue(AnnotationEntry annotationEntry, String... annotationAttributeNames) {
        for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
            ElementValue elementValue = elementValuePair.getValue();
            if (!StringUtils.equalsAny(elementValuePair.getNameString(), annotationAttributeNames)) {
                continue;
            }

            if (elementValue instanceof SimpleElementValue) {
                return elementValue.toString();
            }

            if (elementValue instanceof ClassElementValue) {
                ClassElementValue classElementValue = (ClassElementValue) elementValue;
                return Utility.typeSignatureToString(classElementValue.getClassString(), false);
            }

            throw new JavaCGRuntimeException("注解属性类型不符合预期 " + elementValuePair.getNameString() + " " + elementValue.getClass().getName());
        }
        return null;
    }

    /**
     * 获取注解中指定名称的注解属性值，String数组类型
     * 找到一个符合预期的注解属性名称就返回
     *
     * @param annotationEntry
     * @param annotationAttributeNames
     * @return
     */
    public static List<String> getAnnotationAttributeStringArrayValue(AnnotationEntry annotationEntry, String... annotationAttributeNames) {
        List<String> stringList = new ArrayList<>();
        for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
            if (StringUtils.equalsAny(elementValuePair.getNameString(), annotationAttributeNames)) {
                ElementValue elementValue = elementValuePair.getValue();
                if (!(elementValue instanceof ArrayElementValue)) {
                    throw new JavaCGRuntimeException("注解属性类型不符合预期 " + elementValuePair.getNameString() + " " + elementValue.getClass().getName());
                }
                ArrayElementValue arrayElementValue = (ArrayElementValue) elementValue;
                for (ElementValue elementValueOfArray : arrayElementValue.getElementValuesArray()) {
                    if (!(elementValueOfArray instanceof SimpleElementValue)) {
                        throw new JavaCGRuntimeException("注解属性类型不符合预期 " + elementValuePair.getNameString() + " " + elementValue.getClass().getName());
                    }
                    SimpleElementValue simpleElementValue = (SimpleElementValue) elementValueOfArray;
                    stringList.add(simpleElementValue.toString());
                }

                return stringList;
            }
        }
        return stringList;
    }

    /**
     * 将注解信息写入文件
     *
     * @param classOrMethod     类名或方法
     * @param annotationEntries
     * @param writer
     */
    public static void writeAnnotationInfo(String classOrMethod,
                                           AnnotationEntry[] annotationEntries,
                                           AnnotationAttributesFormatterInterface annotationAttributesFormatter,
                                           Writer writer) {
        if (annotationEntries == null || annotationEntries.length == 0) {
            return;
        }

        try {
            for (AnnotationEntry annotationEntry : annotationEntries) {
                String annotationClassName = Utility.typeSignatureToString(annotationEntry.getAnnotationType(), false);
                if (annotationEntry.getElementValuePairs() == null || annotationEntry.getElementValuePairs().length == 0) {
                    // 注解属性为空
                    JavaCGFileUtil.write2FileWithTab(writer, classOrMethod, annotationClassName);
                    continue;
                }

                // 注解属性非空
                for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
                    String formattedValue = annotationAttributesFormatter.format(elementValuePair);
                    JavaCGFileUtil.write2FileWithTab(writer, classOrMethod, annotationClassName, elementValuePair.getNameString(), formattedValue);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private JavaCGAnnotationUtil() {
        throw new IllegalStateException("illegal");
    }
}
