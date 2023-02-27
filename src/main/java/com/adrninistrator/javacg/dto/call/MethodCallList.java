package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.dto.counter.JavaCGCounter;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/2/7
 * @description: 方法之间调用关系列表
 */
public class MethodCallList {

    // 保存方法之间调用关系
    private final List<MethodCall> methodCallList = new ArrayList<>(50);

    // 保存方法调用计数器
    private final JavaCGCounter callIdCounter;

    public MethodCallList(JavaCGCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void addMethodCall(MethodCall methodCall) {
        methodCall.setCallId(callIdCounter.addAndGet());
        methodCallList.add(methodCall);
    }

    public List<MethodCall> getMethodCallList() {
        return methodCallList;
    }
}
