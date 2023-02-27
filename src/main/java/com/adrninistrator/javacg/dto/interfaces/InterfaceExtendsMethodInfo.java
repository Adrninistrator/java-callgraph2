package com.adrninistrator.javacg.dto.interfaces;

import com.adrninistrator.javacg.dto.method.MethodAndArgs;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/12
 * @description: 接口继承的信息，包括接口继承的接口，及接口中的方法
 */
public class InterfaceExtendsMethodInfo {
    // 接口继承的接口
    private final List<String> superInterfaceList;

    // 接口中的方法
    private final List<MethodAndArgs> methodAndArgsList;

    public InterfaceExtendsMethodInfo(List<String> superInterfaceList, List<MethodAndArgs> methodAndArgsList) {
        this.superInterfaceList = superInterfaceList;
        this.methodAndArgsList = methodAndArgsList;
    }

    public List<String> getSuperInterfaceList() {
        return superInterfaceList;
    }

    public List<MethodAndArgs> getMethodAndArgsList() {
        return methodAndArgsList;
    }
}
