package com.adrninistrator.javacg2.dto.variabledatasource;

import com.adrninistrator.javacg2.dto.element.BaseElement;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/6/13
 * @description: 变量的数据来源，方法调用返回值
 */
public class VariableDataSourceMethodCallReturn extends AbstractVariableDataSource {

    // 方法调用指令位置
    private final int invokeInstructionPosition;

    // 方法调用指令类型的名称（简单类名）
    private final String invokeInstructionType;

    // 被调用类名
    private final String calleeClassName;

    // 被调用方法名
    private final String calleeMethodName;

    // 被调用方法参数类型字符串（包括括号）
    private final String calleeArgTypeStr;

    // 返回类型
    private final String returnType;

    // 被调用对象对应的元素，可能为null
    private final BaseElement objectElement;

    // 方法调用参数对应的元素列表
    private final List<BaseElement> argElementList;

    public VariableDataSourceMethodCallReturn(int invokeInstructionPosition, String invokeInstructionType, String calleeClassName, String calleeMethodName,
                                              String calleeArgTypeStr, String returnType, BaseElement objectElement, List<BaseElement> argElementList) {
        this.invokeInstructionPosition = invokeInstructionPosition;
        this.invokeInstructionType = invokeInstructionType;
        this.calleeClassName = calleeClassName;
        this.calleeMethodName = calleeMethodName;
        this.calleeArgTypeStr = calleeArgTypeStr;
        this.returnType = returnType;
        this.objectElement = objectElement;
        this.argElementList = argElementList;
    }

    /**
     * 比较与另一个对象是否相同
     *
     * @param added
     * @return false: 不相同 true: 相同
     */
    public boolean compare(VariableDataSourceMethodCallReturn added) {
        // 每次处理的方法调用在同一个调用方法内，只需要比较方法调用指令位置
        return this.invokeInstructionPosition == added.invokeInstructionPosition;
    }

    //
    public int getInvokeInstructionPosition() {
        return invokeInstructionPosition;
    }

    public String getInvokeInstructionType() {
        return invokeInstructionType;
    }

    public String getCalleeClassName() {
        return calleeClassName;
    }

    public String getCalleeMethodName() {
        return calleeMethodName;
    }

    public String getCalleeArgTypeStr() {
        return calleeArgTypeStr;
    }

    public String getReturnType() {
        return returnType;
    }

    public BaseElement getObjectElement() {
        return objectElement;
    }

    public List<BaseElement> getArgElementList() {
        return argElementList;
    }

    @Override
    public String toString() {
        return "VariableDataSourceMethodCallReturn{" +
                "invokeInstructionType='" + invokeInstructionType + '\'' +
                ", calleeClassName='" + calleeClassName + '\'' +
                ", calleeMethodName='" + calleeMethodName + '\'' +
                ", calleeArgTypeStr='" + calleeArgTypeStr + '\'' +
                '}';
    }
}
