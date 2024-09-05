package com.adrninistrator.javacg2.dto.classes;

/**
 * @author adrninistrator
 * @date 2024/7/15
 * @description: 类的签名中的泛型信息
 */
public class ClassSignatureGenericsInfo {

    // 类的签名中的泛型的父类类名
    private final String extendsClassName;

    // 类的签名中的泛型序号
    private final int seq;

    public ClassSignatureGenericsInfo(String extendsClassName, int seq) {
        this.extendsClassName = extendsClassName;
        this.seq = seq;
    }

    public String getExtendsClassName() {
        return extendsClassName;
    }

    public int getSeq() {
        return seq;
    }
}
