package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.dto.element.BaseElement;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/3
 * @description: 方法调用可能的信息
 */
public class MethodCallPossibleInformation {
    // 被调用对象可能的信息
    private MethodCallPossibleInfoEntry methodCallPossibleInfoEntry4Object;

    /*
        参数可能的信息
        key
            参数的序号，从0开始
        value
            方法调用中被调用对象、参数的可能的信息
     */
    private Map<Integer, MethodCallPossibleInfoEntry> methodCallPossibleInfoEntryMap4Args;

    /**
     * 为被调用对象添加可能的信息
     *
     * @param baseElement     操作数栈中的元素
     * @param calleeClassName 方法调用指令中被调用对象类名
     */
    public void addPossibleInfo4Object(BaseElement baseElement, String calleeClassName) {
        if (methodCallPossibleInfoEntry4Object == null) {
            methodCallPossibleInfoEntry4Object = new MethodCallPossibleInfoEntry();
        }
        methodCallPossibleInfoEntry4Object.addPossibleInfo(baseElement, calleeClassName);
    }

    /**
     * 为参数添加可能的信息
     *
     * @param seq          参数序号，从0开始
     * @param baseElement  操作数栈中的元素
     * @param argClassName 方法调用指令中参数类名
     */
    public void addPossibleInfo4Args(int seq, BaseElement baseElement, String argClassName) {
        if (methodCallPossibleInfoEntryMap4Args == null) {
            methodCallPossibleInfoEntryMap4Args = new HashMap<>();
        }

        MethodCallPossibleInfoEntry methodCallPossibleInfoEntry = methodCallPossibleInfoEntryMap4Args.computeIfAbsent(seq, k -> new MethodCallPossibleInfoEntry());
        methodCallPossibleInfoEntry.addPossibleInfo(baseElement, argClassName);
    }

    /**
     * 获取被调用对象可能的信息
     *
     * @return
     */
    public MethodCallPossibleInfoEntry getPossibleInfo4Object() {
        return methodCallPossibleInfoEntry4Object;
    }

    /**
     * 获取参数可能的信息
     *
     * @param seq 序号
     * @return
     */
    public MethodCallPossibleInfoEntry getPossibleInfo4Args(int seq) {
        if (methodCallPossibleInfoEntryMap4Args == null) {
            return null;
        }
        return methodCallPossibleInfoEntryMap4Args.get(seq);
    }

    /**
     * 获取参数可能的信息的数量
     *
     * @return
     */
    public int getPossibleInfoNum4Args() {
        if (methodCallPossibleInfoEntryMap4Args == null) {
            return 0;
        }
        return methodCallPossibleInfoEntryMap4Args.size();
    }
}
