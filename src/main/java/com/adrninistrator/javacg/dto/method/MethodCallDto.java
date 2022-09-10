package com.adrninistrator.javacg.dto.method;

import com.adrninistrator.javacg.common.JavaCGConstants;

/**
 * @author adrninistrator
 * @date 2021/7/26
 * @description:
 */

public class MethodCallDto {

    // 调用类型
    private String callType;

    // 当前行的方法调用信息
    private String methodCall;

    // 当前调用者源代码行号
    private int sourceLine;

    // 当前行的调用者完整方法
    private String callerFullMethod;

    public static MethodCallDto genInstance4Method(String methodCall, int sourceLine, String callerFullMethod) {
        return genInstance(JavaCGConstants.FILE_KEY_METHOD_PREFIX, methodCall, sourceLine, callerFullMethod);
    }

    public static MethodCallDto genInstance4Class(String methodCall, int sourceLine) {
        return genInstance(JavaCGConstants.FILE_KEY_CLASS_PREFIX, methodCall, sourceLine, null);
    }

    private static MethodCallDto genInstance(String callType, String methodCall, int sourceLine, String callerFullMethod) {
        MethodCallDto methodCallDto = new MethodCallDto();
        methodCallDto.setCallType(callType);
        methodCallDto.setMethodCall(methodCall);
        methodCallDto.setSourceLine(sourceLine);
        methodCallDto.setCallerFullMethod(callerFullMethod);
        return methodCallDto;
    }

    //
    public String getCallType() {
        return callType;
    }

    public void setCallType(String callType) {
        this.callType = callType;
    }

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

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
    }
}
