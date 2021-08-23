package com.adrninistrator.javacg.dto;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/6/27
 * @description:
 */

public class ChildrenClassInfo {

    private String superClassName;

    private List<String> childrenClassNameList;

    public String getSuperClassName() {
        return superClassName;
    }

    public void setSuperClassName(String superClassName) {
        this.superClassName = superClassName;
    }

    public List<String> getChildrenClassNameList() {
        return childrenClassNameList;
    }

    public void setChildrenClassNameList(List<String> childrenClassNameList) {
        this.childrenClassNameList = childrenClassNameList;
    }
}
