package com.adrninistrator.javacg.dto.classes;

/**
 * @author adrninistrator
 * @date 2021/6/27
 * @description: 处理涉及继承的类的方法时使用的信息
 */

public class Node4ClassExtendsMethod {
    // 父类类名
    private final String superClassName;

    // 当前处理的子类下标
    private int childClassIndex;

    public Node4ClassExtendsMethod(String superClassName, int childClassIndex) {
        this.superClassName = superClassName;
        this.childClassIndex = childClassIndex;
    }

    // get
    public String getSuperClassName() {
        return superClassName;
    }

    public int getChildClassIndex() {
        return childClassIndex;
    }

    // set
    public void setChildClassIndex(int childClassIndex) {
        this.childClassIndex = childClassIndex;
    }
}
