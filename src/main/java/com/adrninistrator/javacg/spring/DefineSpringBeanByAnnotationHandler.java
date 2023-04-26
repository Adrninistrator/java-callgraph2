package com.adrninistrator.javacg.spring;

import com.adrninistrator.javacg.common.SpringAnnotationConstants;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.handler.MethodHandler4TypeAndValue;
import com.adrninistrator.javacg.util.JavaCGAnnotationUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/10/3
 * @description: 处理通过注解定义的Spring Bean信息
 */
public class DefineSpringBeanByAnnotationHandler {

    private final JavaCGConfInfo javaCGConfInfo;

    /*
        保存Spring Bean信息
            key:    Bean的名称
            value:  Bean的类型列表
     */
    private final Map<String, List<String>> stringBeanNameAndTypeMap = new HashMap<>(100);

    public DefineSpringBeanByAnnotationHandler(JavaCGConfInfo javaCGConfInfo) {
        this.javaCGConfInfo = javaCGConfInfo;
    }

    /**
     * 记录通过注解定义的Spring Bean信息
     *
     * @param javaClass
     */
    public boolean recordSpringBeanInfo(JavaClass javaClass) {
        for (AnnotationEntry annotationEntry : javaClass.getAnnotationEntries()) {
            String annotationType = annotationEntry.getAnnotationType();
            // 判断是否为Spring Component相关注解
            if (isSpringComponentAnnotation(annotationType)) {
                // 处理类上的Spring Component相关注解
                handleSpringComponentAnnotation(annotationEntry, javaClass.getClassName());
                continue;
            }

            if (SpringAnnotationConstants.ANNOTATION_NAME_CONFIGURATION.equals(annotationType)) {
                // 处理类上的Spring Configuration注解
                if (!handleSpringConfigurationAnnotation(javaClass)) {
                    return false;
                }
            }
        }
        return true;
    }

    // 判断是否为Spring Component相关注解
    private boolean isSpringComponentAnnotation(String annotationType) {
        for (String annotation : SpringAnnotationConstants.SPRING_COMPONENT_ANNOTATIONS_RAW) {
            if (annotation.equals(annotationType)) {
                return true;
            }
        }
        // 再判断是否有@Named注解
        return SpringAnnotationConstants.ANNOTATION_NAME_NAMED.equals(annotationType);
    }

    // 处理类上的Spring Component相关注解
    private void handleSpringComponentAnnotation(AnnotationEntry annotationEntry, String className) {
        // 若Component相关注解的value属性值非空，则作为Bean名称使用
        String valueAttributeValue = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
        if (StringUtils.isNotBlank(valueAttributeValue)) {
            stringBeanNameAndTypeMap.put(valueAttributeValue, Collections.singletonList(className));
            return;
        }

        // 若Component相关注解的value属性值为空
        // 将类名首字母小写作为Bean名称作用
        String simpleClassName = JavaCGUtil.getSimpleClassNameFromFull(className);
        String firstLetterLowerClassName = JavaCGUtil.getFirstLetterLowerClassName(simpleClassName);
        stringBeanNameAndTypeMap.put(firstLetterLowerClassName, Collections.singletonList(className));

        // 再将类名作为Bean名称作用（用来支持Bean使用@Service、@Repository等注解定义，使用@Resource的type属性注入对应Bean）
        stringBeanNameAndTypeMap.put(className, Collections.singletonList(className));
    }

    // 处理类上的Spring Configuration注解
    private boolean handleSpringConfigurationAnnotation(JavaClass javaClass) {
        // 遍历每个方法，若有@Bean注解则处理
        for (Method method : javaClass.getMethods()) {
            for (AnnotationEntry annotationEntry : method.getAnnotationEntries()) {
                String annotationType = annotationEntry.getAnnotationType();
                if (!SpringAnnotationConstants.ANNOTATION_NAME_BEAN.equals(annotationType)) {
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
        MethodGen mg = new MethodGen(method, javaClass.getClassName(), new ConstantPoolGen(javaClass.getConstantPool()));
        MethodHandler4TypeAndValue methodHandler4TypeAndValue = new MethodHandler4TypeAndValue(mg, javaClass, javaCGConfInfo);
        methodHandler4TypeAndValue.setParseMethodCallTypeValueFlag(false);
        methodHandler4TypeAndValue.setRecordReturnPossibleInfoFlag(true);
        if (!methodHandler4TypeAndValue.handleMethod()) {
            return false;
        }
        // 获取方法可能的返回类型列表
        List<String> returnPossibleTypeList = methodHandler4TypeAndValue.getReturnPossibleTypeList();
        if (returnPossibleTypeList == null) {
            return true;
        }

        // 获取@Bean注解的name属性
        List<String> nameAttributeValueList = JavaCGAnnotationUtil.getAnnotationAttributeStringArrayValue(beanAnnotation,
                SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_NAME, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
        if (!nameAttributeValueList.isEmpty()) {
            // @Bean注解的name属性非空时，作为bean的名称
            for (String nameAttributeValue : nameAttributeValueList) {
                // 记录@Bean对应的bean名称及类型列表
                stringBeanNameAndTypeMap.put(nameAttributeValue, returnPossibleTypeList);
            }
            return true;
        }
        // @Bean注解的name属性为空时，方法名作为bean的名称
        // 记录@Bean对应的bean名称及类型列表
        stringBeanNameAndTypeMap.put(method.getName(), returnPossibleTypeList);

        return true;
    }

    /**
     * 根据Spring Bean的名称获取类型列表
     *
     * @param beanName
     * @return
     */
    public List<String> getSpringBeanTypeList(String beanName) {
        return stringBeanNameAndTypeMap.get(beanName);
    }

    /**
     * 获取Spring Bean的名称Set
     *
     * @return
     */
    public Set<String> getSpringBeanNameSet() {
        return stringBeanNameAndTypeMap.keySet();
    }
}
