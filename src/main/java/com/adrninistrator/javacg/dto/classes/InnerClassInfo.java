package com.adrninistrator.javacg.dto.classes;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description: 内部类信息
 */
public class InnerClassInfo {
    // 内部类名
    private final String innerClassName;

    // 外部类名
    private final String outerClassName;

    // 是否为匿名内部类
    private final boolean anonymousClass;

    public InnerClassInfo(String innerClassName, String outerClassName, boolean anonymousClass) {
        this.innerClassName = innerClassName;
        this.outerClassName = outerClassName;
        this.anonymousClass = anonymousClass;
    }

    public String getInnerClassName() {
        return innerClassName;
    }

    public String getOuterClassName() {
        return outerClassName;
    }

    public boolean isAnonymousClass() {
        return anonymousClass;
    }
}
