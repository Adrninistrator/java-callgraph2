package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg.dto.field.StaticFieldTypeAndName;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2023/3/12
 * @description: 方法调用中被调用对象、参数的可能的信息
 */
public class MethodCallPossibleEntry {

    // 可能的被调用静态字段
    private StaticFieldTypeAndName staticField;

    // 可能的被调用非静态字段
    private FieldTypeAndName nonStaticField;

    // 可能的类型
    private String type;

    // 可能的值的类型
    private String valueType;

    // 可能的值
    private Object value;

    // 被调用对象或参数是静态字段方法返回值的可能信息
    private String staticFieldMethodCall;

    // 被调用对象或参数变量名称
    private String nameOfVariable;

    // 被调用对象或参数来自方法调用返回，对应的完整方法
    private String methodCallReturnFullMethod;

    // 被调用对象或参数来自方法调用返回，对应的方法调用指令的位置
    private Integer methodCallReturnInstructionPosition;

    // 方法参数的序号
    private Integer methodArgSeq;

    // 被调用对象或参数来自等值转换前的方法调用返回，对应的完整方法
    private String methodCallReturnFullMethodEQC;

    // 被调用对象或参数来自等值转换前的方法调用返回，对应的方法调用指令的位置
    private Integer methodCallReturnInstructionPositionEQC;

    // 等值转换前的方法参数的序号
    private Integer methodArgSeqEQC;

    // catch异常对象对应的catch代码块开始指令偏移量
    private Integer catchExceptionStartPosition;

    // 以上增加字段后需要在compare()方法中增加对应的判断

    // 记录被设置的内容数量
    private int contentNum = 0;

    public StaticFieldTypeAndName getStaticField() {
        return staticField;
    }

    public void setStaticField(StaticFieldTypeAndName staticField) {
        this.staticField = staticField;
        addContentNum();
    }

    /**
     * 比较与另一个需要添加的对象值是否相同
     *
     * @param added
     * @return
     */
    public boolean compare(MethodCallPossibleEntry added) {
        // 当前方法执行前有判断需要添加的对象值是否有内容，这里不需要判断，直接比较内容就可以
        return Objects.equals(staticField, added.staticField)
                && Objects.equals(nonStaticField, added.nonStaticField)
                && StringUtils.equals(type, added.type)
                && Objects.equals(value, added.value)
                && StringUtils.equals(staticFieldMethodCall, added.staticFieldMethodCall)
                && StringUtils.equals(nameOfVariable, added.nameOfVariable)
                && StringUtils.equals(methodCallReturnFullMethod, added.methodCallReturnFullMethod)
                && Objects.equals(methodCallReturnInstructionPosition, added.methodCallReturnInstructionPosition)
                && Objects.equals(methodArgSeq, added.methodArgSeq)
                && Objects.equals(methodCallReturnInstructionPositionEQC, added.methodCallReturnInstructionPositionEQC)
                && Objects.equals(methodArgSeqEQC, added.methodArgSeqEQC)
                && Objects.equals(catchExceptionStartPosition, added.catchExceptionStartPosition);
    }

    public boolean hasContent() {
        return contentNum > 0;
    }

    private void addContentNum() {
        contentNum++;
    }

    public FieldTypeAndName getNonStaticField() {
        return nonStaticField;
    }

    public void setNonStaticField(FieldTypeAndName nonStaticField) {
        this.nonStaticField = nonStaticField;
        addContentNum();
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
        addContentNum();
    }

    public String getValueType() {
        return valueType;
    }

    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
        addContentNum();
    }

    public String getStaticFieldMethodCall() {
        return staticFieldMethodCall;
    }

    public void setStaticFieldMethodCall(String staticFieldMethodCall) {
        this.staticFieldMethodCall = staticFieldMethodCall;
        addContentNum();
    }

    public String getNameOfVariable() {
        return nameOfVariable;
    }

    public void setNameOfVariable(String nameOfVariable) {
        this.nameOfVariable = nameOfVariable;
        addContentNum();
    }

    public String getMethodCallReturnFullMethod() {
        return methodCallReturnFullMethod;
    }

    public void setMethodCallReturnFullMethod(String methodCallReturnFullMethod) {
        this.methodCallReturnFullMethod = methodCallReturnFullMethod;
        // 不增加内容数量
    }

    public Integer getMethodCallReturnInstructionPosition() {
        return methodCallReturnInstructionPosition;
    }

    public void setMethodCallReturnInstructionPosition(Integer methodCallReturnInstructionPosition) {
        this.methodCallReturnInstructionPosition = methodCallReturnInstructionPosition;
        addContentNum();
    }

    public Integer getMethodArgSeq() {
        return methodArgSeq;
    }

    public void setMethodArgSeq(Integer methodArgSeq) {
        this.methodArgSeq = methodArgSeq;
        addContentNum();
    }

    public String getMethodCallReturnFullMethodEQC() {
        return methodCallReturnFullMethodEQC;
    }

    public void setMethodCallReturnFullMethodEQC(String methodCallReturnFullMethodEQC) {
        this.methodCallReturnFullMethodEQC = methodCallReturnFullMethodEQC;
        // 不增加内容数量
    }

    public Integer getMethodCallReturnInstructionPositionEQC() {
        return methodCallReturnInstructionPositionEQC;
    }

    public void setMethodCallReturnInstructionPositionEQC(Integer methodCallReturnInstructionPositionEQC) {
        this.methodCallReturnInstructionPositionEQC = methodCallReturnInstructionPositionEQC;
        addContentNum();
    }

    public Integer getMethodArgSeqEQC() {
        return methodArgSeqEQC;
    }

    public void setMethodArgSeqEQC(Integer methodArgSeqEQC) {
        this.methodArgSeqEQC = methodArgSeqEQC;
        addContentNum();
    }

    public Integer getCatchExceptionStartPosition() {
        return catchExceptionStartPosition;
    }

    public void setCatchExceptionStartPosition(Integer catchExceptionStartPosition) {
        this.catchExceptionStartPosition = catchExceptionStartPosition;
        addContentNum();
    }
}
