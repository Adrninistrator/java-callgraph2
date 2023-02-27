package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description: 方法之间调用关系
 */
public class MethodCall {

    // 方法调用序号
    private int callId;

    // 调用者类名
    private String callerClassName;

    // 调用者方法名
    private String callerMethodName;

    // 调用者方法参数
    private String callerMethodArgs;

    // 方法调用类型
    private CallTypeEnum methodCallType;

    // 被调用者类名
    private String calleeClassName;

    // 被调用者方法名
    private String calleeMethodName;

    // 被调用者方法参数
    private String calleeMethodArgs;

    // 调用者源代码行号
    private int callerSourceLine;

    public MethodCall(int callId,
                      String callerClassName,
                      String callerMethodName,
                      String callerMethodArgs,
                      CallTypeEnum methodCallType,
                      String calleeClassName,
                      String calleeMethodName,
                      String calleeMethodArgs,
                      int callerSourceLine) {
        this(callerClassName,
                callerMethodName,
                callerMethodArgs,
                methodCallType,
                calleeClassName,
                calleeMethodName,
                calleeMethodArgs,
                callerSourceLine);
        this.callId = callId;
    }

    public MethodCall(String callerClassName,
                      String callerMethodName,
                      String callerMethodArgs,
                      CallTypeEnum methodCallType,
                      String calleeClassName,
                      String calleeMethodName,
                      String calleeMethodArgs,
                      int callerSourceLine) {
        this.callerClassName = callerClassName;
        this.callerMethodName = callerMethodName;
        this.callerMethodArgs = callerMethodArgs;
        this.methodCallType = methodCallType;
        this.calleeClassName = calleeClassName;
        this.calleeMethodName = calleeMethodName;
        this.calleeMethodArgs = calleeMethodArgs;
        this.callerSourceLine = callerSourceLine;
    }

    // 返回调用者完整方法
    public String genCallerFullMethod() {
        return JavaCGUtil.formatFullMethod(callerClassName, callerMethodName, callerMethodArgs);
    }

    // 返回被调用者完整方法
    public String genCalleeFullMethod() {
        return JavaCGUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeMethodArgs);
    }

    public String genCallContent() {
        return StringUtils.joinWith(JavaCGConstants.FILE_COLUMN_SEPARATOR,
                callId,
                genCallerFullMethod(),
                JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1 + methodCallType.getType() + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2 + genCalleeFullMethod(),
                callerSourceLine);
    }

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public String getCallerClassName() {
        return callerClassName;
    }

    public void setCallerClassName(String callerClassName) {
        this.callerClassName = callerClassName;
    }

    public String getCallerMethodName() {
        return callerMethodName;
    }

    public void setCallerMethodName(String callerMethodName) {
        this.callerMethodName = callerMethodName;
    }

    public String getCallerMethodArgs() {
        return callerMethodArgs;
    }

    public void setCallerMethodArgs(String callerMethodArgs) {
        this.callerMethodArgs = callerMethodArgs;
    }

    public int getCallerSourceLine() {
        return callerSourceLine;
    }

    public void setCallerSourceLine(int callerSourceLine) {
        this.callerSourceLine = callerSourceLine;
    }

    public CallTypeEnum getMethodCallType() {
        return methodCallType;
    }

    public void setMethodCallType(CallTypeEnum methodCallType) {
        this.methodCallType = methodCallType;
    }

    public String getCalleeClassName() {
        return calleeClassName;
    }

    public void setCalleeClassName(String calleeClassName) {
        this.calleeClassName = calleeClassName;
    }

    public String getCalleeMethodName() {
        return calleeMethodName;
    }

    public void setCalleeMethodName(String calleeMethodName) {
        this.calleeMethodName = calleeMethodName;
    }

    public String getCalleeMethodArgs() {
        return calleeMethodArgs;
    }

    public void setCalleeMethodArgs(String calleeMethodArgs) {
        this.calleeMethodArgs = calleeMethodArgs;
    }
}
