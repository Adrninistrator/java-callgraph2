package com.adrninistrator.javacg2.dto.classes;

/**
 * @author adrninistrator
 * @date 2024/7/10
 * @description: 类涉及继承的信息，包含类的accessFlags，父类
 */
public class ClassExtendsInfo {

    // 类的的accessFlags
    private final int accessFlags;

    // 父类名称
    private final String superClassName;

    public ClassExtendsInfo(int accessFlags, String superClassName) {
        this.accessFlags = accessFlags;
        this.superClassName = superClassName;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public String getSuperClassName() {
        return superClassName;
    }
}
