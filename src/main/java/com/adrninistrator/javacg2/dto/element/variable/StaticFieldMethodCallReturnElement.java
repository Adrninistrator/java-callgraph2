package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.bcel.generic.Type;

import java.util.Arrays;

/**
 * @author adrninistrator
 * @date 2022/11/11
 * @description: 静态字段的方法调用返回值
 */
public class StaticFieldMethodCallReturnElement extends LocalVariableElement {

    /*
        父类中的type字段为被调用的方法返回类型
        父类中的name字段为被调用的字段名称
     */

    // 被调用的类名
    private final String className;

    // 被调用的字段类型
    private final String fieldType;

    // 被调用的方法名称
    private final String methodName;

    // 被调用的方法参数类型
    private final Type[] methodArgTypes;

    // type字段为被调用的方法返回类型
    public StaticFieldMethodCallReturnElement(String type, int addArrayDimensions, String className, String fieldName, String fieldType, String methodName, Type[] methodArgTypes) {
        super(type, addArrayDimensions, null, 0, fieldName);
        this.className = className;
        this.fieldType = fieldType;
        this.methodName = methodName;
        this.methodArgTypes = methodArgTypes;
    }

    @Override
    public BaseElement copyElement() {
        StaticFieldMethodCallReturnElement staticFieldMethodCallReturnElementCopy = new StaticFieldMethodCallReturnElement(getType(), 0, className, getName(), fieldType,
                methodName, methodArgTypes);
        staticFieldMethodCallReturnElementCopy.copyVariableDataSource(this);
        staticFieldMethodCallReturnElementCopy.setArrayValueMap(getArrayValueMap());
        return staticFieldMethodCallReturnElementCopy;
    }

    public String getDetailInfo() {
        return JavaCG2ClassMethodUtil.formatClassFieldMethodArgTypes(className, getName(), fieldType, methodName, methodArgTypes, getType());
    }

    public String getClassName() {
        return className;
    }

    public String getFieldType() {
        return fieldType;
    }

    public String getMethodName() {
        return methodName;
    }

    public Type[] getMethodArgTypes() {
        return methodArgTypes;
    }

    @Override
    public String toString() {
        return "StaticFieldMethodCallReturnElement{" +
                "simpleClassName='" + simpleClassName + '\'' +
                ", type='" + getType() + '\'' +
                ", value=" + value +
                ", className='" + className + '\'' +
                ", name='" + getName() + '\'' +
                ", fieldType='" + fieldType + '\'' +
                ", methodName='" + methodName + '\'' +
                ", argTypes='" + Arrays.toString(methodArgTypes) + '\'' +
                '}';
    }
}
