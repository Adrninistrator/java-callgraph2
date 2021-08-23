package com.adrninistrator.javacg.dto;

/**
 * @author adrninistrator
 * @date 2021/6/27
 * @description:
 */

public class TmpNode4ExtendsClassMethod {

    private String superClassName;

    private int childClassIndex;

    public static TmpNode4ExtendsClassMethod genInstance(String superClassName, int childClassIndex) {
        TmpNode4ExtendsClassMethod tmpNode = new TmpNode4ExtendsClassMethod();
        tmpNode.setSuperClassName(superClassName);
        tmpNode.setChildClassIndex(childClassIndex);
        return tmpNode;
    }

    public String getSuperClassName() {
        return superClassName;
    }

    public void setSuperClassName(String superClassName) {
        this.superClassName = superClassName;
    }

    public int getChildClassIndex() {
        return childClassIndex;
    }

    public void setChildClassIndex(int childClassIndex) {
        this.childClassIndex = childClassIndex;
    }
}
