package com.adrninistrator.javacg2.spring;

import com.adrninistrator.javacg2.common.SpringAnnotationConstants;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import com.adrninistrator.javacg2.util.JavaCG2SpringUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.JavaClass;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/6/22
 * @description: 处理通过注解定义的Spring包扫描路径
 */
public class SpringDefineComponentScanHandler {

    /*
        保存Spring包扫描路径信息
        key:    @ComponentScan注解所在类名
        value:  对应包扫描路径列表
     */
    private final Map<String, List<String>> springComponentScanMap = new HashMap<>();

    /**
     * 处理通过注解定义的Spring包扫描路径
     *
     * @param javaClass
     */
    public void recordSpringInfo(JavaClass javaClass) {
        boolean existsConfiguration = false;
        for (AnnotationEntry annotationEntry : javaClass.getAnnotationEntries()) {
            String annotationType = annotationEntry.getAnnotationType();
            if (JavaCG2SpringUtil.checkConfigurationAnnotation(annotationType)) {
                // 类上有指定Spring Configuration注解
                existsConfiguration = true;
                break;
            }
        }
        if (!existsConfiguration) {
            return;
        }

        // 类上存在@Configuration注解，判断是否存在@ComponentScan注解，若存在则处理
        for (AnnotationEntry annotationEntry : javaClass.getAnnotationEntries()) {
            String annotationType = annotationEntry.getAnnotationType();
            if (JavaCG2SpringUtil.checkComponentScanAnnotation(annotationType)) {
                // 处理ComponentScan注解
                handleComponentScanAnnotation(javaClass, annotationEntry);
            }
        }
    }

    // 处理ComponentScan注解
    private void handleComponentScanAnnotation(JavaClass javaClass, AnnotationEntry componentScanAnnotation) {
        // 获取ComponentScan注解的basePackages或value属性值
        List<String> basePackagesAttributeValueList = JavaCG2AnnotationUtil.getAnnotationAttributeStringArrayValue(componentScanAnnotation,
                SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_BASE_PACKAGES, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
        if (JavaCG2Util.isCollectionEmpty(basePackagesAttributeValueList)) {
            // ComponentScan注解的basePackages或value属性值为空，使用类所在的包名
            basePackagesAttributeValueList = Collections.singletonList(javaClass.getPackageName());
        }
        springComponentScanMap.put(javaClass.getClassName(), basePackagesAttributeValueList);
    }

    public Map<String, List<String>> getSpringComponentScanMap() {
        return springComponentScanMap;
    }
}
