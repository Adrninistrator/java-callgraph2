package com.adrninistrator.javacg.extension;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description:
 */
public class CustomData {
    private int callId;

    private String dataType;

    private String dataValue;

    public static CustomData genCustomData(int callId, String dataType, String dataValue) {
        CustomData customData = new CustomData();
        customData.setCallId(callId);
        customData.setDataType(dataType);
        customData.setDataValue(dataValue);
        return customData;
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
