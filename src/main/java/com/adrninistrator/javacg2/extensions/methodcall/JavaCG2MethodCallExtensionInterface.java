package com.adrninistrator.javacg2.extensions.methodcall;

import com.adrninistrator.javacg2.dto.call.MethodCall;
import com.adrninistrator.javacg2.dto.call.MethodCallList;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description: 方法调用处理扩展类接口
 */
public interface JavaCG2MethodCallExtensionInterface {

    /**
     * 处理方法调用
     *
     * @param methodCall     当前的方法调用
     * @param callIdCounter  方法调用计数器
     * @param methodCallList 方法调用列表
     */
    void handle(MethodCall methodCall, JavaCG2Counter callIdCounter, MethodCallList methodCallList);
}
