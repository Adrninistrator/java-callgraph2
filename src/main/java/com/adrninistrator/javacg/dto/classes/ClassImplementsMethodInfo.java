package com.adrninistrator.javacg.dto.classes;

import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/6/21
 * @description: 类实现的接口，及类中的方法信息
 */
public class ClassImplementsMethodInfo {

    // 类实现的接口
    private final List<String> interfaceNameList;

    // 类中的方法信息
    private final List<MethodArgReturnTypes> methodWithArgsList;

    public ClassImplementsMethodInfo(List<String> interfaceNameList, List<MethodArgReturnTypes> methodWithArgsList) {
        this.interfaceNameList = interfaceNameList;
        this.methodWithArgsList = methodWithArgsList;
    }

    public List<String> getInterfaceNameList() {
        return interfaceNameList;
    }

    public List<MethodArgReturnTypes> getMethodWithArgsList() {
        return methodWithArgsList;
    }
}
