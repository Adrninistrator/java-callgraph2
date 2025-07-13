package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.SpringAnnotationConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2025/6/24
 * @description:
 */
public class JavaCG2SpringUtil {

    /**
     * 判断是否为Spring Component相关注解，或@Named注解
     *
     * @param annotationType 注解的原始类型
     * @return
     */
    public static boolean checkComponentAnnotation(String annotationType) {
        for (String annotation : SpringAnnotationConstants.ANNOTATION_TYPE_COMPONENTS) {
            if (annotation.equals(annotationType)) {
                return true;
            }
        }
        // 再判断是否有@Named注解
        return SpringAnnotationConstants.ANNOTATION_TYPE_NAMED.equals(annotationType);
    }

    /**
     * 判断是否为Spring ComponentScan注解
     *
     * @param annotationType 注解的原始类型
     * @return
     */
    public static boolean checkComponentScanAnnotation(String annotationType) {
        return SpringAnnotationConstants.ANNOTATION_TYPE_COMPONENT_SCAN.equals(annotationType);
    }

    /**
     * 判断是否为Configuration注解
     *
     * @param annotationType
     * @return
     */
    public static boolean checkConfigurationAnnotation(String annotationType) {
        return SpringAnnotationConstants.ANNOTATION_TYPE_CONFIGURATION.equals(annotationType);
    }

    /**
     * 判断是否为Bean注解
     *
     * @param annotationType
     * @return
     */
    public static boolean checkBeanAnnotation(String annotationType) {
        return SpringAnnotationConstants.ANNOTATION_TYPE_BEAN.equals(annotationType);
    }

    /**
     * 判断是否为Aspect注解
     *
     * @param annotationType
     * @return
     */
    public static boolean checkAspectAnnotation(String annotationType) {
        return SpringAnnotationConstants.ANNOTATION_TYPE_ASPECT.equals(annotationType);
    }

    /**
     * 判断是否为Order注解
     *
     * @param annotationType
     * @return
     */
    public static boolean checkOrderAnnotation(String annotationType) {
        return SpringAnnotationConstants.ANNOTATION_TYPE_ORDER.equals(annotationType);
    }

    /**
     * 判断是否为Pointcut注解
     *
     * @param annotationType
     * @return
     */
    public static boolean checkPointcutAnnotation(String annotationType) {
        return SpringAnnotationConstants.ANNOTATION_TYPE_POINTCUT.equals(annotationType);
    }

    /**
     * 判断是否为Advice相关注解
     *
     * @param annotationType
     * @return
     */
    public static boolean checkAdviceAnnotation(String annotationType) {
        return StringUtils.equalsAny(annotationType, SpringAnnotationConstants.ANNOTATION_TYPE_ADVICES);
    }

    private JavaCG2SpringUtil() {
        throw new IllegalStateException("illegal");
    }
}
