package com.adrninistrator.javacg2.dto.exception;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description: 代码中抛出的可能的异常信息列表
 */
public class ThrowInfoList {

    private final List<ThrowInfo> throwInfoList = new ArrayList<>(1);

    public void addThrowInfo(ThrowInfo throwInfo) {
        if (!throwInfoList.contains(throwInfo)) {
            throwInfoList.add(throwInfo);
        }
    }

    public int size() {
        return throwInfoList.size();
    }

    public ThrowInfo get(int i) {
        return throwInfoList.get(i);
    }
}
