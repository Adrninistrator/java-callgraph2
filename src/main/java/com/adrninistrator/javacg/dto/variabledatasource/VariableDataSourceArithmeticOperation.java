package com.adrninistrator.javacg.dto.variabledatasource;

import com.adrninistrator.javacg.common.enums.JavaCGArithmeticOperationTypeEnum;
import com.adrninistrator.javacg.dto.element.BaseElement;

/**
 * @author adrninistrator
 * @date 2023/8/5
 * @description: 变量的数据来源，算术运算
 */
public class VariableDataSourceArithmeticOperation extends AbstractVariableDataSource {

    // 算术运算指令位置
    private final int operationInstructionPosition;

    // 算术运算类型
    private final JavaCGArithmeticOperationTypeEnum arithmeticOperationTypeEnum;

    // 参与运算的第1个元素
    private final BaseElement element1;

    // 参与运算的第2个元素
    private final BaseElement element2;

    public VariableDataSourceArithmeticOperation(int operationInstructionPosition, JavaCGArithmeticOperationTypeEnum arithmeticOperationTypeEnum, BaseElement element1,
                                                 BaseElement element2) {
        this.operationInstructionPosition = operationInstructionPosition;
        this.arithmeticOperationTypeEnum = arithmeticOperationTypeEnum;
        this.element1 = element1;
        this.element2 = element2;
    }

    /**
     * 比较与另一个对象是否相同
     *
     * @param added
     * @return false: 不相同 true: 相同
     */
    public boolean compare(VariableDataSourceArithmeticOperation added) {
        // 每次处理的方法调用在同一个调用方法内，只需要比较方法调用指令位置
        return this.operationInstructionPosition == added.operationInstructionPosition;
    }

    public int getOperationInstructionPosition() {
        return operationInstructionPosition;
    }

    public JavaCGArithmeticOperationTypeEnum getArithmeticOperationTypeEnum() {
        return arithmeticOperationTypeEnum;
    }

    public BaseElement getElement1() {
        return element1;
    }

    public BaseElement getElement2() {
        return element2;
    }

    @Override
    public String toString() {
        return "VariableDataSourceArithmeticOperation{" +
                "invokeInstructionPosition=" + operationInstructionPosition +
                ", arithmeticOperationTypeEnum=" + arithmeticOperationTypeEnum +
                ", element1=" + element1 +
                ", element2=" + element2 +
                '}';
    }
}
