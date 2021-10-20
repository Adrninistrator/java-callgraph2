package com.adrninistrator.javacg.extensions.dto;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description:
 */
public class ExtendedData {
    private int callId;

    private String dataType;

    private String dataValue;

    public static ExtendedData genExtendedData(int callId, String dataType, String dataValue) {
        ExtendedData extendedData = new ExtendedData();
        extendedData.setCallId(callId);
        extendedData.setDataType(dataType);
        extendedData.setDataValue(dataValue);
        return extendedData;
    }

    //
    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public String getDataValue() {
        return dataValue;
    }

    public void setDataValue(String dataValue) {
        this.dataValue = dataValue;
    }
}
