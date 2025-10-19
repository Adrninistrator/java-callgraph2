package com.adrninistrator.javacg2.dto.call;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeObjTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description: 方法之间调用关系
 */
public class MethodCall {

    // 方法调用序号
    private int callId;

    // 方法调用是否启用
    private boolean enabled;

    // 方法调用类型
    private String methodCallType;

    // 调用者类名
    private String callerClassName;

    // 调用者方法名
    private String callerMethodName;

    // 调用者方法参数类型
    private String callerMethodArgTypes;

    // 调用者源代码行号
    private int callerSourceLine;

    // 调用者方法返回类型
    private String callerReturnType;

    // 被调用者类名
    private String calleeClassName;

    // 被调用者方法名
    private String calleeMethodName;

    // 被调用者方法参数类型
    private String calleeMethodArgTypes;

    // 被调用方法的参数类型数组，原始的
    private Type[] rawCalleeArgTypes;

    // 被调用方法的参数类型数组，用于显示的
    private Type[] displayCalleeArgTypes;

    // 被调用者类的数组维度，0代表非数组
    private int calleeArrayDimensions;

    // 被调用类型
    private JavaCG2CalleeObjTypeEnum calleeObjTypeEnum;

    // 原始返回类型
    private String rawReturnType;

    // 实际返回类型
    private String actualReturnType;

    // 描述
    private String description;

    //
    // 返回调用者完整方法
    public String genCallerFullMethod() {
        return JavaCG2ClassMethodUtil.formatFullMethod(callerClassName, callerMethodName, callerMethodArgTypes);
    }

    // 返回被调用类型对应的字符串
    public String genObjTypeEnum() {
        if (calleeObjTypeEnum == null) {
            return "";
        }
        return calleeObjTypeEnum.getType();
    }

    // 返回被调用者完整方法
    public String genCalleeFullMethod() {
        return JavaCG2ClassMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeMethodArgTypes);
    }

    // 生成在调用关系文件中的内容
    public String genMethodCallContent(String callerJarNum, String calleeJarNum) {
        return JavaCG2FileUtil.appendFileColumn(
                String.valueOf(callId),
                enabled ? JavaCG2YesNoEnum.YES.getStrValue() : JavaCG2YesNoEnum.NO.getStrValue(),
                genCallerFullMethod(),
                JavaCG2Constants.FILE_KEY_CALL_TYPE_FLAG1 + methodCallType + JavaCG2Constants.FILE_KEY_CALL_TYPE_FLAG2 + genCalleeFullMethod(),
                String.valueOf(callerSourceLine),
                callerReturnType,
                String.valueOf(calleeArrayDimensions),
                genObjTypeEnum(),
                rawReturnType,
                actualReturnType,
                callerJarNum,
                calleeJarNum,
                description == null ? "" : description
        );
    }

    //
    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String getMethodCallType() {
        return methodCallType;
    }

    public void setMethodCallType(String methodCallType) {
        this.methodCallType = methodCallType;
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

    public int getCallerSourceLine() {
        return callerSourceLine;
    }

    public void setCallerSourceLine(int callerSourceLine) {
        this.callerSourceLine = callerSourceLine;
    }

    public String getCallerReturnType() {
        return callerReturnType;
    }

    public void setCallerReturnType(String callerReturnType) {
        this.callerReturnType = callerReturnType;
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

    public Type[] getRawCalleeArgTypes() {
        return rawCalleeArgTypes;
    }

    public void setRawCalleeArgTypes(Type[] rawCalleeArgTypes) {
        this.rawCalleeArgTypes = rawCalleeArgTypes;
    }

    public Type[] getDisplayCalleeArgTypes() {
        return displayCalleeArgTypes;
    }

    public void setDisplayCalleeArgTypes(Type[] displayCalleeArgTypes) {
        this.displayCalleeArgTypes = displayCalleeArgTypes;
    }

    public int getCalleeArrayDimensions() {
        return calleeArrayDimensions;
    }

    public void setCalleeArrayDimensions(int calleeArrayDimensions) {
        this.calleeArrayDimensions = calleeArrayDimensions;
    }

    public JavaCG2CalleeObjTypeEnum getCalleeObjTypeEnum() {
        return calleeObjTypeEnum;
    }

    public void setCalleeObjTypeEnum(JavaCG2CalleeObjTypeEnum calleeObjTypeEnum) {
        this.calleeObjTypeEnum = calleeObjTypeEnum;
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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
