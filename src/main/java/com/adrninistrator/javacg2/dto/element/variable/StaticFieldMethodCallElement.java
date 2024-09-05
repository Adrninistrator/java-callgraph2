package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2022/11/11
 * @description: 静态字段的方法调用
 */
public class StaticFieldMethodCallElement extends VariableElement {

    // 类名
    private final String className;

    // 字段名称
    private final String fieldName;

    // 调用的方法名称
    private final String methodName;

    public StaticFieldMethodCallElement(String type, boolean arrayElement, String className, String fieldName, String methodName) {
        super(type, arrayElement);
        this.className = className;
        this.fieldName = fieldName;
        this.methodName = methodName;
    }

    @Override
    public BaseElement copyElement() {
        StaticFieldMethodCallElement staticFieldMethodCallElementCopy = new StaticFieldMethodCallElement(getType(), arrayElement, className, fieldName, methodName);
        staticFieldMethodCallElementCopy.copyVariableDataSource(this);
        return staticFieldMethodCallElementCopy;
    }

    public String getInfo() {
        return JavaCG2ClassMethodUtil.genClassAndField(className, fieldName) + JavaCG2Constants.FLAG_COLON + methodName + JavaCG2Constants.EMPTY_METHOD_ARGS;
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
                '}';
    }
}
