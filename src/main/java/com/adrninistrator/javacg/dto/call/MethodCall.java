package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGCalleeObjTypeEnum;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.bcel.generic.Type;

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

    // 调用者方法参数类型
    private String callerMethodArgTypes;

    // 调用者方法返回类型
    private String callerReturnType;

    // 方法调用类型
    private JavaCGCallTypeEnum methodCallType;

    // 被调用者类名
    private String calleeClassName;

    // 被调用者方法名
    private String calleeMethodName;

    // 被调用者方法参数类型
    private String calleeMethodArgTypes;

    // 调用者源代码行号
    private int callerSourceLine;

    // 被调用类型
    private JavaCGCalleeObjTypeEnum objTypeEnum;

    // 原始返回类型
    private String rawReturnType;

    // 实际返回类型
    private String actualReturnType;

    // 被调用方法的参数类型数组
    private Type[] argTypes;

    // 返回调用者完整方法
    public String genCallerFullMethod() {
        return JavaCGClassMethodUtil.formatFullMethod(callerClassName, callerMethodName, callerMethodArgTypes);
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
        return JavaCGClassMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeMethodArgTypes);
    }

    // 生成在调用关系文件中的内容
    public String genMethodCallContent(String callerJarNum, String calleeJarNum) {
        return JavaCGFileUtil.appendFileColumn(
                String.valueOf(callId),
                genCallerFullMethod(),
                JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG1 + methodCallType.getType() + JavaCGConstants.FILE_KEY_CALL_TYPE_FLAG2 + genCalleeFullMethod(),
                String.valueOf(callerSourceLine),
                callerReturnType,
                genObjTypeEnum(),
                rawReturnType,
                actualReturnType,
                callerJarNum,
                calleeJarNum
        );
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

    public String getCallerMethodArgTypes() {
        return callerMethodArgTypes;
    }

    public void setCallerMethodArgTypes(String callerMethodArgTypes) {
        this.callerMethodArgTypes = callerMethodArgTypes;
    }

    public String getCallerReturnType() {
        return callerReturnType;
    }

    public void setCallerReturnType(String callerReturnType) {
        this.callerReturnType = callerReturnType;
    }

    public JavaCGCallTypeEnum getMethodCallType() {
        return methodCallType;
    }

    public void setMethodCallType(JavaCGCallTypeEnum methodCallType) {
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

    public String getCalleeMethodArgTypes() {
        return calleeMethodArgTypes;
    }

    public void setCalleeMethodArgTypes(String calleeMethodArgTypes) {
        this.calleeMethodArgTypes = calleeMethodArgTypes;
    }

    public int getCallerSourceLine() {
        return callerSourceLine;
    }

    public void setCallerSourceLine(int callerSourceLine) {
        this.callerSourceLine = callerSourceLine;
    }

    public JavaCGCalleeObjTypeEnum getObjTypeEnum() {
        return objTypeEnum;
    }

    public void setObjTypeEnum(JavaCGCalleeObjTypeEnum objTypeEnum) {
        this.objTypeEnum = objTypeEnum;
    }

    public String getRawReturnType() {
        return rawReturnType;
    }

    public void setRawReturnType(String rawReturnType) {
        this.rawReturnType = rawReturnType;
    }

    public String getActualReturnType() {
        return actualReturnType;
    }

    public void setActualReturnType(String actualReturnType) {
        this.actualReturnType = actualReturnType;
    }

    public Type[] getArgTypes() {
        return argTypes;
    }

    public void setArgTypes(Type[] argTypes) {
        this.argTypes = argTypes;
    }
}
