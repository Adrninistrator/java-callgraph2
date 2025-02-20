package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.bcel.generic.Type;

import java.util.Arrays;

/**
 * @author adrninistrator
 * @date 2022/11/11
 * @description: 静态字段的方法调用
 */
public class StaticFieldMethodCallElement extends VariableElement {

    // 被调用的类名
    private final String className;

    // 被调用的字段名称
    private final String fieldName;

    // 被调用的方法名称
    private final String methodName;

    // 被调用的方法参数类型
    private final Type[] argTypes;

    public StaticFieldMethodCallElement(String type, int addArrayDimensions, String className, String fieldName, String methodName, Type[] argTypes) {
        super(type, addArrayDimensions);
        this.className = className;
        this.fieldName = fieldName;
        this.methodName = methodName;
        this.argTypes = argTypes;
    }

    @Override
    public BaseElement copyElement() {
        StaticFieldMethodCallElement staticFieldMethodCallElementCopy = new StaticFieldMethodCallElement(getType(), 0, className, fieldName, methodName, argTypes);
        staticFieldMethodCallElementCopy.copyVariableDataSource(this);
        staticFieldMethodCallElementCopy.setArrayValueMap(getArrayValueMap());
        return staticFieldMethodCallElementCopy;
    }

    public String getInfo() {
        return JavaCG2ClassMethodUtil.formatClassFieldMethodArgTypes(className, fieldName, methodName, argTypes);
    }

    @Override
    public String toString() {
        return "StaticFieldMethodCallElement{" +
                "simpleClassName='" + simpleClassName + '\'' +
                ", type='" + getType() + '\'' +
                ", value=" + value +
                ", className='" + className + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", methodName='" + methodName + '\'' +
                ", argTypes='" + Arrays.toString(argTypes) + '\'' +
                '}';
    }
}
