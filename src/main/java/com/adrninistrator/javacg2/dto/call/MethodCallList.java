package com.adrninistrator.javacg2.dto.call;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

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
    private final JavaCG2Counter callIdCounter;

    public MethodCallList(JavaCG2Counter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    /**
     * 增加方法调用对象，使用指定的的call_id
     *
     * @param methodCall
     */
    public void addMethodCall(MethodCall methodCall) {
        if (methodCall == null || methodCall.getCallId() < JavaCG2Constants.METHOD_CALL_ID_MIN) {
            throw new JavaCG2RuntimeException("不允许传入null，或者是call_id非法");
        }
        methodCallList.add(methodCall);
    }

    /**
     * 增加方法调用对象，使用自动增加的call_id
     *
     * @param methodCall
     */
    public void addMethodCallAutoCallId(MethodCall methodCall) {
        if (methodCall == null || methodCall.getCallId() >= JavaCG2Constants.METHOD_CALL_ID_MIN) {
            throw new JavaCG2RuntimeException("不允许传入null，或者是call_id有值");
        }
        methodCall.setCallId(callIdCounter.addAndGet());
        methodCallList.add(methodCall);
    }

    public List<MethodCall> getMethodCallList() {
        return methodCallList;
    }
}
