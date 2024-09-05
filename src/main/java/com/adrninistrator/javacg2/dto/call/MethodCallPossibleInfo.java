package com.adrninistrator.javacg2.dto.call;

import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeObjTypeEnum;
import com.adrninistrator.javacg2.dto.element.BaseElement;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/3
 * @description: 方法调用可能的信息，包括对应的被调用对象及参数
 */
public class MethodCallPossibleInfo {

    // 被调用类型
    private JavaCG2CalleeObjTypeEnum objTypeEnum;

    // 被调用对象可能的信息
    private MethodCallPossibleList methodCallPossibleList4Object;

    /*
        参数可能的信息
        key
            参数的序号，从0开始
        value
            方法调用中被调用对象、参数的可能的信息
     */
    private Map<Integer, MethodCallPossibleList> methodCallPossibleListMap4Args;

    public JavaCG2CalleeObjTypeEnum getObjTypeEnum() {
        return objTypeEnum;
    }

    public void setObjTypeEnum(JavaCG2CalleeObjTypeEnum objTypeEnum) {
        this.objTypeEnum = objTypeEnum;
    }

    /**
     * 为被调用对象添加可能的信息
     *
     * @param baseElement     操作数栈中的元素
     * @param calleeClassName 方法调用指令中被调用对象类名
     */
    public void addPossibleInfo4Object(BaseElement baseElement, String calleeClassName) {
        if (methodCallPossibleList4Object == null) {
            methodCallPossibleList4Object = new MethodCallPossibleList();
        }
        List<BaseElement> handledElementList = new ArrayList<>(1);
        methodCallPossibleList4Object.addPossibleInfo(baseElement, calleeClassName, handledElementList);
    }

    /**
     * 为参数添加可能的信息
     *
     * @param seq          参数序号，从0开始
     * @param baseElement  操作数栈中的元素
     * @param argClassName 方法调用指令中参数类名
     */
    public void addPossibleInfo4Args(int seq, BaseElement baseElement, String argClassName) {
        if (methodCallPossibleListMap4Args == null) {
            methodCallPossibleListMap4Args = new HashMap<>();
        }

        MethodCallPossibleList methodCallPossibleList = methodCallPossibleListMap4Args.computeIfAbsent(seq, k -> new MethodCallPossibleList());
        List<BaseElement> handledElementList = new ArrayList<>(1);
        methodCallPossibleList.addPossibleInfo(baseElement, argClassName, handledElementList);
    }

    /**
     * 获取被调用对象可能的信息
     *
     * @return
     */
    public MethodCallPossibleList getPossibleInfo4Object() {
        return methodCallPossibleList4Object;
    }

    /**
     * 获取参数可能的信息
     *
     * @param seq 参数序号，从0开始
     * @return
     */
    public MethodCallPossibleList getPossibleInfo4Args(int seq) {
        if (methodCallPossibleListMap4Args == null) {
            return null;
        }
        return methodCallPossibleListMap4Args.get(seq);
    }

    /**
     * 获取参数可能的信息的数量
     *
     * @return
     */
    public int getPossibleInfoNum4Args() {
        if (methodCallPossibleListMap4Args == null) {
            return 0;
        }
        return methodCallPossibleListMap4Args.size();
    }
}
