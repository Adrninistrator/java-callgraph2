package com.adrninistrator.javacg2.dto.exception;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description: 方法中抛出的异常信息
 */
public class ThrowInfo {

    // 抛出的异常类型
    private final String throwExceptionType;

    // 抛出异常的标志
    private final String throwFlag;

    // 抛出异常属于catch的异常对象时，对应的catch代码块开始指令偏移量
    private final Integer catchStartPosition;

    // 抛出异常属于catch的异常对象时，对应catch的异常对象变量名称（以下equals、hashCode方法可以不判断该字段）
    private final String catchExceptionVariableName;

    // 抛出异常属于方法调用返回值时，对应的方法调用指令偏移量
    private final Integer invokeInstructionPosition;

    public ThrowInfo(String throwExceptionType, String throwFlag, Integer catchStartPosition, String catchExceptionVariableName, Integer invokeInstructionPosition) {
        this.throwExceptionType = throwExceptionType;
        this.throwFlag = throwFlag;
        this.catchStartPosition = catchStartPosition;
        this.catchExceptionVariableName = catchExceptionVariableName;
        this.invokeInstructionPosition = invokeInstructionPosition;
    }

    public String getThrowExceptionType() {
        return throwExceptionType;
    }

    public String getThrowFlag() {
        return throwFlag;
    }

    public Integer getCatchStartPosition() {
        return catchStartPosition;
    }

    public String getCatchExceptionVariableName() {
        return catchExceptionVariableName;
    }

    public Integer getInvokeInstructionPosition() {
        return invokeInstructionPosition;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ThrowInfo throwInfo = (ThrowInfo) o;
        return throwExceptionType.equals(throwInfo.throwExceptionType) && throwFlag.equals(throwInfo.throwFlag) && Objects.equals(catchStartPosition,
                throwInfo.catchStartPosition) && Objects.equals(invokeInstructionPosition, throwInfo.invokeInstructionPosition);
    }

    @Override
    public int hashCode() {
        return Objects.hash(throwExceptionType, throwFlag, catchStartPosition, invokeInstructionPosition);
    }

    @Override
    public String toString() {
        return "ThrowInfo{" +
                "throwExceptionType='" + throwExceptionType + '\'' +
                ", throwFlag='" + throwFlag + '\'' +
                ", catchStartPosition=" + catchStartPosition +
                ", invokeInstructionPosition=" + invokeInstructionPosition +
                '}';
    }
}
