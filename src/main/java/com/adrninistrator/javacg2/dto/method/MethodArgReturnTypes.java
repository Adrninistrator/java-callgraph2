package com.adrninistrator.javacg2.dto.method;

import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.bcel.generic.Type;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2022/10/1
 * @description: 方法名称、方法参数与返回类型
 */
public class MethodArgReturnTypes {
    // 方法名称
    private final String methodName;

    // 方法参数类型字符串
    private final String methodArgTypes;

    // 方法返回类型
    private final String methodReturnType;

    public MethodArgReturnTypes(String methodName, Type[] argTypes, Type returnType) {
        this.methodName = methodName;
        this.methodArgTypes = JavaCG2ClassMethodUtil.getArgTypeStr(argTypes);
        this.methodReturnType = returnType.toString();
    }

    @Override
    public String toString() {
        return "MethodArgReturnTypes{" +
                "methodName='" + methodName + '\'' +
                ", methodArgTypes='" + methodArgTypes + '\'' +
                ", methodReturnType='" + methodReturnType + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MethodArgReturnTypes that = (MethodArgReturnTypes) o;
        return methodName.equals(that.methodName) && methodArgTypes.equals(that.methodArgTypes) && methodReturnType.equals(that.methodReturnType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(methodName, methodArgTypes, methodReturnType);
    }

    public String getMethodName() {
        return methodName;
    }

    public String getMethodArgTypes() {
        return methodArgTypes;
    }

    public String getMethodReturnType() {
        return methodReturnType;
    }
}
