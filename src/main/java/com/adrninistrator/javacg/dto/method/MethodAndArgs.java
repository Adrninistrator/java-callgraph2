package com.adrninistrator.javacg.dto.method;

import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import org.apache.bcel.generic.Type;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2022/10/1
 * @description: 方法名及方法参数
 */
public class MethodAndArgs {
    private String methodName;

    private String methodArgs;

    public MethodAndArgs(String methodName, Type[] argTypes) {
        this.methodName = methodName;
        this.methodArgs = JavaCGByteCodeUtil.getArgListStr(argTypes);
    }

    public MethodAndArgs(String methodName, String methodArgs) {
        this.methodName = methodName;
        this.methodArgs = methodArgs;
    }

    @Override
    public String toString() {
        return methodName + methodArgs;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MethodAndArgs that = (MethodAndArgs) o;
        return Objects.equals(methodName, that.methodName) && Objects.equals(methodArgs, that.methodArgs);
    }

    @Override
    public int hashCode() {
        return Objects.hash(methodName, methodArgs);
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getMethodArgs() {
        return methodArgs;
    }

    public void setMethodArgs(String methodArgs) {
        this.methodArgs = methodArgs;
    }
}
