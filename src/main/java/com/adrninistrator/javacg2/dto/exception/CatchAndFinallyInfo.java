package com.adrninistrator.javacg2.dto.exception;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/12/28
 * @description: Exception table中某个try对应的catch、finally信息
 */
public class CatchAndFinallyInfo {

    private final List<CatchInfo> catchInfoList = new ArrayList<>();

    private FinallyInfo finallyInfo;

    public List<CatchInfo> getCatchInfoList() {
        return catchInfoList;
    }

    public FinallyInfo getFinallyInfo() {
        return finallyInfo;
    }

    public void setFinallyInfo(FinallyInfo finallyInfo) {
        this.finallyInfo = finallyInfo;
    }
}
