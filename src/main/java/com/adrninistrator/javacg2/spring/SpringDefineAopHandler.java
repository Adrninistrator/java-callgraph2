package com.adrninistrator.javacg2.spring;

import com.adrninistrator.javacg2.common.SpringAnnotationConstants;
import com.adrninistrator.javacg2.dto.spring.SpringAopAdviceInfo;
import com.adrninistrator.javacg2.dto.spring.SpringAopPointcutInfo;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2SpringUtil;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.Utility;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/6/24
 * @description: 处理通过注解定义的Spring Aop信息
 */
public class SpringDefineAopHandler {

    /*
        aspect信息的Map
        key 有@Aspect注解的类名
        value   类的@Order注解的value
     */
    private final Map<String, Integer> aspectMap = new HashMap<>();

    /*
        pointcut信息的Map
        key 有@Aspect注解的类名
        value   对应类中的pointcut列表
     */
    private final Map<String, List<SpringAopPointcutInfo>> pointcutMap = new HashMap<>();

    /*
        advice信息的Map
        key 有@Aspect注解的类名
        value   对应类中的advice列表
     */
    private final Map<String, List<SpringAopAdviceInfo>> adviceMap = new HashMap<>();

    /**
     * 处理通过注解定义的Spring包扫描路径
     *
     * @param javaClass
     */
    public void recordSpringInfo(JavaClass javaClass) {
        boolean existsComponent = false;
        boolean existsAspect = false;
        String orderValueStr = null;
        for (AnnotationEntry annotationEntry : javaClass.getAnnotationEntries()) {
            String annotationType = annotationEntry.getAnnotationType();
            if (JavaCG2SpringUtil.checkComponentAnnotation(annotationType)) {
                // 类上有指定Spring Component等注解
                existsComponent = true;
            } else if (JavaCG2SpringUtil.checkAspectAnnotation(annotationType)) {
                // 类上有指定Spring Aspect注解
                existsAspect = true;
            } else if (JavaCG2SpringUtil.checkOrderAnnotation(annotationType)) {
                // 类上有指定Spring Order注解
                orderValueStr = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
            }
        }
//        if (!existsComponent || !existsAspect) {
//            // 类上不是同时存在@Component等注解，及Aspect注解，跳过
//            return;
//        }
        if (!existsAspect) {
            // 先只判断类上是否存在Aspect注解，不存在时跳过
            return;
        }

        // 类上存在@Component等注解，及Aspect注解
        String className = javaClass.getClassName();
        int orderValue = Integer.MAX_VALUE;
        if (orderValueStr != null) {
            orderValue = Integer.parseInt(orderValueStr);
        }
        // 记录有@Aspect注解的类
        aspectMap.put(className, orderValue);

        // 处理方法上的注解
        for (Method method : javaClass.getMethods()) {
            // 处理方法的注解
            handleMethodAnnotation(className, method, orderValue);
        }
    }

    // 处理方法的注解
    private void handleMethodAnnotation(String className, Method method, int aspectOrder) {
        for (AnnotationEntry annotationEntry : method.getAnnotationEntries()) {
            String annotationType = annotationEntry.getAnnotationType();
            if (JavaCG2SpringUtil.checkPointcutAnnotation(annotationType)) {
                String fullMethod = JavaCG2ClassMethodUtil.formatFullMethod(className, method);
                String expression = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);

                List<SpringAopPointcutInfo> springAopPointInfoList = pointcutMap.computeIfAbsent(className, k -> new ArrayList<>());
                SpringAopPointcutInfo springAopPointcutInfo = new SpringAopPointcutInfo();
                springAopPointcutInfo.setFullMethod(fullMethod);
                springAopPointcutInfo.setExpression(expression);
                springAopPointInfoList.add(springAopPointcutInfo);
            } else if (JavaCG2SpringUtil.checkAdviceAnnotation(annotationType)) {
                String fullMethod = JavaCG2ClassMethodUtil.formatFullMethod(className, method);
                String expression = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
                String pointcut = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(annotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_POINTCUT);
                String usedExpression = pointcut != null ? pointcut : expression;
                String annotationClassName = Utility.typeSignatureToString(annotationType, false);
                String annotationSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(annotationClassName);

                List<SpringAopAdviceInfo> springAopAdviceInfoList = adviceMap.computeIfAbsent(className, k -> new ArrayList<>());
                SpringAopAdviceInfo springAopAdviceInfo = new SpringAopAdviceInfo();
                springAopAdviceInfo.setAnnotationSimpleClassName(annotationSimpleClassName);
                springAopAdviceInfo.setFullMethod(fullMethod);
                springAopAdviceInfo.setReturnType(method.getReturnType().toString());
                springAopAdviceInfo.setExpression(usedExpression);
                springAopAdviceInfo.setAspectOrder(aspectOrder);
                springAopAdviceInfoList.add(springAopAdviceInfo);
            }
        }
    }

    public Map<String, Integer> getAspectMap() {
        return aspectMap;
    }

    public Map<String, List<SpringAopPointcutInfo>> getPointcutMap() {
        return pointcutMap;
    }

    public Map<String, List<SpringAopAdviceInfo>> getAdviceMap() {
        return adviceMap;
    }
}
