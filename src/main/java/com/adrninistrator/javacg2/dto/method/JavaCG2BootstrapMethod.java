package com.adrninistrator.javacg2.dto.method;

import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2025/7/24
 * @description:
 */
public class JavaCG2BootstrapMethod {

    // bootstrap方法信息
    private JavaCG2MethodInfo bootstrapMethodInfo;

    // lambda表达式方法信息
    private JavaCG2MethodInfo lambdaMethodInfo;

    // lambda表达式方法实际参数类型
    private Type[] lambdaMethodActualArgumentTypes;

    // lambda表达式方法实际返回类型
    private Type lambdaMethodActualReturnType;

    public JavaCG2MethodInfo getBootstrapMethodInfo() {
        return bootstrapMethodInfo;
    }

    public void setBootstrapMethodInfo(JavaCG2MethodInfo bootstrapMethodInfo) {
        this.bootstrapMethodInfo = bootstrapMethodInfo;
    }

    public JavaCG2MethodInfo getLambdaMethodInfo() {
        return lambdaMethodInfo;
    }

    public void setLambdaMethodInfo(JavaCG2MethodInfo lambdaMethodInfo) {
        this.lambdaMethodInfo = lambdaMethodInfo;
    }

    public Type[] getLambdaMethodActualArgumentTypes() {
        return lambdaMethodActualArgumentTypes;
    }

    public void setLambdaMethodActualArgumentTypes(Type[] lambdaMethodActualArgumentTypes) {
        this.lambdaMethodActualArgumentTypes = lambdaMethodActualArgumentTypes;
    }

    public Type getLambdaMethodActualReturnType() {
        return lambdaMethodActualReturnType;
    }

    public void setLambdaMethodActualReturnType(Type lambdaMethodActualReturnType) {
        this.lambdaMethodActualReturnType = lambdaMethodActualReturnType;
    }
}
