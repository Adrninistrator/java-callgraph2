package com.adrninistrator.javacg2.dto.spring;

/**
 * @author adrninistrator
 * @date 2025/6/27
 * @description:
 */
public class SpringAopAdviceInfo {

    // 完整方法
    private String fullMethod;

    // 方法返回类型
    private String returnType;

    // advice注解简单类
    private String annotationSimpleClassName;

    // advice的表达式
    private String expression;

    // aspect的排序值
    private int aspectOrder;

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public String getAnnotationSimpleClassName() {
        return annotationSimpleClassName;
    }

    public void setAnnotationSimpleClassName(String annotationSimpleClassName) {
        this.annotationSimpleClassName = annotationSimpleClassName;
    }

    public String getExpression() {
        return expression;
    }

    public void setExpression(String expression) {
        this.expression = expression;
    }

    public int getAspectOrder() {
        return aspectOrder;
    }

    public void setAspectOrder(int aspectOrder) {
        this.aspectOrder = aspectOrder;
    }
}
