package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGCalleeObjTypeEnum;
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
    private JavaCGCallTypeEnum methodCallType;

    // 被调用者类名
    private String calleeClassName;

    // 被调用者方法名
    private String calleeMethodName;

    // 被调用者方法参数
    private String calleeMethodArgs;

    // 调用者源代码行号
    private int callerSourceLine;

    // 被调用类型
    private JavaCGCalleeObjTypeEnum objTypeEnum;

    public MethodCall(int callId,
                      String callerClassName,
                      String callerMethodName,
                      String callerMethodArgs,
                      JavaCGCallTypeEnum methodCallType,
                      String calleeClassName,
                      String calleeMethodName,
                      String calleeMethodArgs,
                      int callerSourceLine,
                      JavaCGCalleeObjTypeEnum objTypeEnum) {
        this(callerClassName,
                callerMethodName,
                callerMethodArgs,
                methodCallType,
                calleeClassName,
                calleeMethodName,
                calleeMethodArgs,
                callerSourceLine,
                objTypeEnum);
        this.callId = callId;
    }

    public MethodCall(String callerClassName,
                      String callerMethodName,
                      String callerMethodArgs,
                      JavaCGCallTypeEnum methodCallType,
                      String calleeClassName,
                      String calleeMethodName,
                      String calleeMethodArgs,
                      int callerSourceLine,
                      JavaCGCalleeObjTypeEnum objTypeEnum) {
        this.callerClassName = callerClassName;
        this.callerMethodName = callerMethodName;
        this.callerMethodArgs = callerMethodArgs;
        this.methodCallType = methodCallType;
        this.calleeClassName = calleeClassName;
        this.calleeMethodName = calleeMethodName;
        this.calleeMethodArgs = calleeMethodArgs;
        this.callerSourceLine = callerSourceLine;
        this.objTypeEnum = objTypeEnum;
    }

    // 返回调用者完整方法
    public String genCallerFullMethod() {
        return JavaCGUtil.formatFullMethod(callerClassName, callerMethodName, callerMethodArgs);
    }

    // 返回被调用类型对应的字符串
    public String genObjTypeEnum() {
        if (objTypeEnum == null) {
            return "";
        }
        return objTypeEnum.getType();
    }

    // 返回被调用者完整方法
    public String genCalleeFullMethod() {
        return JavaCGUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeMethodArgs);
    }

    // 生成在调用关系文件中的内容
    public String genCallContent() {
        return StringUtils.joinWith(JavaCGConstants.FILE_COLUMN_SEPARATOR,
                callId,
                genCallerFullMethod(),
                JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1 + methodCallType.getType() + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2 + genCalleeFullMethod(),
                callerSourceLine,
                genObjTypeEnum()
        );
    }

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }
}
