package com.adrninistrator.javacg.dto.fieldrelationship;

import com.adrninistrator.javacg.common.enums.JavaCGFieldRelationshipTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/10/16
 * @description: get/set方法对应的字段关系
 */
public class GetSetFieldRelationship {

    // 记录ID
    private int recordId;

    // set方法调用的指令位置
    private Integer setInvokeInstructionPosition;

    // get方法调用的指令位置
    private Integer getInvokeInstructionPosition;

    // 调用方，源代码行号
    private int callerLineNumber;

    // get方法完整类名
    private String getClassName;

    // get方法方法名
    private String getMethodName;

    // set方法完整类名
    private String setClassName;

    // set方法方法名
    private String setMethodName;

    // 关联关系是否有效
    private String valid;

    // 关联关系类型
    private JavaCGFieldRelationshipTypeEnum type;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public Integer getSetInvokeInstructionPosition() {
        return setInvokeInstructionPosition;
    }

    public void setSetInvokeInstructionPosition(Integer setInvokeInstructionPosition) {
        this.setInvokeInstructionPosition = setInvokeInstructionPosition;
    }

    public Integer getGetInvokeInstructionPosition() {
        return getInvokeInstructionPosition;
    }

    public void setGetInvokeInstructionPosition(Integer getInvokeInstructionPosition) {
        this.getInvokeInstructionPosition = getInvokeInstructionPosition;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(int callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public String getGetClassName() {
        return getClassName;
    }

    public void setGetClassName(String getClassName) {
        this.getClassName = getClassName;
    }

    public String getGetMethodName() {
        return getMethodName;
    }

    public void setGetMethodName(String getMethodName) {
        this.getMethodName = getMethodName;
    }

    public String getSetClassName() {
        return setClassName;
    }

    public void setSetClassName(String setClassName) {
        this.setClassName = setClassName;
    }

    public String getSetMethodName() {
        return setMethodName;
    }

    public void setSetMethodName(String setMethodName) {
        this.setMethodName = setMethodName;
    }

    public String getValid() {
        return valid;
    }

    public void setValid(String valid) {
        this.valid = valid;
    }

    public JavaCGFieldRelationshipTypeEnum getType() {
        return type;
    }

    public void setType(JavaCGFieldRelationshipTypeEnum type) {
        this.type = type;
    }
}
