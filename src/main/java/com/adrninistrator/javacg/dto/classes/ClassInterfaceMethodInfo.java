package com.adrninistrator.javacg.dto.classes;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/6/21
 * @description:
 */

public class ClassInterfaceMethodInfo {

    private List<String> interfaceNameList;

    private List<String> methodWithArgsList;

    public List<String> getInterfaceNameList() {
        return interfaceNameList;
    }

    public void setInterfaceNameList(List<String> interfaceNameList) {
        this.interfaceNameList = interfaceNameList;
    }

    public List<String> getMethodWithArgsList() {
        return methodWithArgsList;
    }

    public void setMethodWithArgsList(List<String> methodWithArgsList) {
        this.methodWithArgsList = methodWithArgsList;
    }
}
