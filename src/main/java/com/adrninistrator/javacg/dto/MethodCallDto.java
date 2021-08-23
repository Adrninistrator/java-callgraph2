package com.adrninistrator.javacg.dto;

/**
 * @author adrninistrator
 * @date 2021/7/26
 * @description:
 */

public class MethodCallDto {

    private String methodCall;

    private int sourceLine;

    public static MethodCallDto genInstance(String methodCall, int sourceLine) {
        MethodCallDto methodCallDto = new MethodCallDto();
        methodCallDto.setMethodCall(methodCall);
        methodCallDto.setSourceLine(sourceLine);
        return methodCallDto;
    }

    //
    public String getMethodCall() {
        return methodCall;
    }

    public void setMethodCall(String methodCall) {
        this.methodCall = methodCall;
    }

    public int getSourceLine() {
        return sourceLine;
    }

    public void setSourceLine(int sourceLine) {
        this.sourceLine = sourceLine;
    }
}
