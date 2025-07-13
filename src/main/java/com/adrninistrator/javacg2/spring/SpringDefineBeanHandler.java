package com.adrninistrator.javacg2.spring;

import com.adrninistrator.javacg2.common.SpringAnnotationConstants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.dto.spring.SpringBeanInJava;
import com.adrninistrator.javacg2.handler.MethodHandler4TypeAndValue;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2SpringUtil;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/10/3
 * @description: 处理通过注解定义的Spring Bean信息
 */
public class SpringDefineBeanHandler {

    private final JavaCG2InputAndOutput javaCG2InputAndOutput;

    private final JavaCG2Counter failCounter;

    /*
        保存Spring Bean信息
            key:    Spring Bean的名称
            value:  Java代码中定义的Spring Bean信息
     */
    private final Map<String, SpringBeanInJava> springBeanInJavaMap = new HashMap<>(100);

    public SpringDefineBeanHandler(JavaCG2InputAndOutput javaCG2InputAndOutput, JavaCG2Counter failCounter) {
        this.javaCG2InputAndOutput = javaCG2InputAndOutput;
        this.failCounter = failCounter;
    }

    /**
     * 处理通过注解定义的Spring Bean信息
     *
     * @param javaClass
     */
    public boolean recordSpringInfo(JavaClass javaClass) {
        for (AnnotationEntry annotationEntry : javaClass.getAnnotationEntries()) {
            String annotationType = annotationEntry.getAnnotationType();
            // 判断是否为Spring Component相关注解
            if (JavaCG2SpringUtil.checkComponentAnnotation(annotationType)) {
                // 处理类上的Spring Component相关注解
                handleSpringComponentAnnotation(annotationEntry, javaClass.getClassName());
                continue;
            }

            if (JavaCG2SpringUtil.checkConfigurationAnnotation(annotationType)) {
                // 处理类上的Spring Configuration注解
                if (!handleSpringConfigurationAnnotation(javaClass)) {
                    return false;
                }
            }
        }
        return true;
    }

    // 处理类上的Spring Component相关注解
    private void handleSpringComponentAnnotation(AnnotationEntry annotationEntry, String className) {
        SpringBeanInJava springBeanInJava = new SpringBeanInJava();
        springBeanInJava.setClassNameList(Collections.singletonList(className));
        String annotationClassName = Utility.typeSignatureToString(annotationEntry.getAnnotationType(), false);
        springBeanInJava.setAnnotationClassName(annotationClassName);
        springBeanInJava.setDefineClassName(className);

        // 若Component相关注解的value属性值非空，则作为Bean名称使用
        String valueAttributeValue = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
        if (StringUtils.isNotBlank(valueAttributeValue)) {
            springBeanInJavaMap.put(valueAttributeValue, springBeanInJava);
            return;
        }

        // 若Component相关注解的value属性值为空
        // 将类名首字母小写作为Bean名称作用
        String simpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
        String firstLetterLowerClassName = JavaCG2ClassMethodUtil.getFirstLetterLowerClassName(simpleClassName);
        springBeanInJavaMap.put(firstLetterLowerClassName, springBeanInJava);
    }

    // 处理类上的Spring Configuration注解
    private boolean handleSpringConfigurationAnnotation(JavaClass javaClass) {
        // 遍历每个方法，若有@Bean注解则处理
        for (Method method : javaClass.getMethods()) {
            for (AnnotationEntry annotationEntry : method.getAnnotationEntries()) {
                String annotationType = annotationEntry.getAnnotationType();
                if (!JavaCG2SpringUtil.checkBeanAnnotation(annotationType)) {
                    continue;
                }

                // 处理方法上的Spring Bean注解
                if (!handleSpringBeanAnnotation(javaClass, method, annotationEntry)) {
                    return false;
                }
            }
        }
        return true;
    }

    // 处理方法上的Spring Bean注解
    private boolean handleSpringBeanAnnotation(JavaClass javaClass, Method method, AnnotationEntry beanAnnotation) {
        String className = javaClass.getClassName();
        MethodGen mg = new MethodGen(method, className, new ConstantPoolGen(javaClass.getConstantPool()));
        String callerFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(className, method.getName(), method.getArgumentTypes());
        MethodHandler4TypeAndValue methodHandler4TypeAndValue = new MethodHandler4TypeAndValue(method, mg, javaClass, callerFullMethod, javaCG2InputAndOutput);
        methodHandler4TypeAndValue.setFailCounter(failCounter);
        methodHandler4TypeAndValue.setParseMethodCallTypeValueFlag(true);
        methodHandler4TypeAndValue.setOnlyAnalyseReturnTypeFlag(true);
        if (!methodHandler4TypeAndValue.handleMethod()) {
            return false;
        }
        // 获取方法可能的返回类型列表
        List<String> returnPossibleTypeList = methodHandler4TypeAndValue.getReturnPossibleTypeList();
        if (returnPossibleTypeList == null) {
            return true;
        }

        SpringBeanInJava springBeanInJava = new SpringBeanInJava();
        springBeanInJava.setClassNameList(returnPossibleTypeList);
        springBeanInJava.setAnnotationClassName(SpringAnnotationConstants.ANNOTATION_CLASS_NAME_BEAN);
        springBeanInJava.setDefineClassName(className);

        // 获取@Bean注解的name属性
        List<String> nameAttributeValueList = JavaCG2AnnotationUtil.getAnnotationAttributeStringArrayValue(beanAnnotation,
                SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_NAME, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
        if (!nameAttributeValueList.isEmpty()) {
            // @Bean注解的name属性非空时，作为bean的名称
            for (String nameAttributeValue : nameAttributeValueList) {
                // 记录@Bean对应的bean名称及类型列表
                springBeanInJavaMap.put(nameAttributeValue, springBeanInJava);
            }
            return true;
        }
        // @Bean注解的name属性为空时，方法名作为bean的名称
        // 记录@Bean对应的bean名称及类型列表
        springBeanInJavaMap.put(method.getName(), springBeanInJava);

        return true;
    }

    public Map<String, SpringBeanInJava> getSpringBeanInJavaMap() {
        return springBeanInJavaMap;
    }
}
