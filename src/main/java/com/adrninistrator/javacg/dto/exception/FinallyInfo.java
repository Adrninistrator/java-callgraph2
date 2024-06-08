package com.adrninistrator.javacg.dto.exception;

/**
 * @author adrninistrator
 * @date 2023/12/28
 * @description: Exception table中的finally信息
 */
public class FinallyInfo {
    protected final int fromPosition;
    protected final int toPosition;
    protected final int endPosition;
    protected final int targetPosition;

    public FinallyInfo(int fromPosition, int toPosition, int endPosition, int targetPosition) {
        this.fromPosition = fromPosition;
        this.toPosition = toPosition;
        this.endPosition = endPosition;
        this.targetPosition = targetPosition;
    }

    public int getFromPosition() {
        return fromPosition;
    }

    public int getToPosition() {
        return toPosition;
    }

    public int getEndPosition() {
        return endPosition;
    }

    public int getTargetPosition() {
        return targetPosition;
    }

    @Override
    public String toString() {
        return "FinallyInfo{" +
                "fromPosition=" + fromPosition +
                ", toPosition=" + toPosition +
                ", endPosition=" + endPosition +
                ", targetPosition=" + targetPosition +
                '}';
    }
}
