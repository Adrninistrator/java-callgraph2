package com.adrninistrator.javacg2.dto.spring;

/**
 * @author adrninistrator
 * @date 2025/6/27
 * @description:
 */
public class SpringAopPointcutInfo {

    // 完整方法
    private String fullMethod;

    // pointcut的表达式
    private String expression;

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getExpression() {
        return expression;
    }

    public void setExpression(String expression) {
        this.expression = expression;
    }
}
