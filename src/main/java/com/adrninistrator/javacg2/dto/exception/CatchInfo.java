package com.adrninistrator.javacg2.dto.exception;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/12/28
 * @description: Exception table中的catch信息
 */
public class CatchInfo extends FinallyInfo {

    private final List<String> catchExceptionTypeList;

    public CatchInfo(int fromPosition, int toPosition, int endPosition, int targetPosition, String catchExceptionType) {
        super(fromPosition, toPosition, endPosition, targetPosition);
        catchExceptionTypeList = new ArrayList<>(1);
        catchExceptionTypeList.add(catchExceptionType);
    }

    public void addCatchExceptionType(String catchExceptionType) {
        catchExceptionTypeList.add(catchExceptionType);
    }

    public List<String> getCatchExceptionTypeList() {
        return catchExceptionTypeList;
    }
}
