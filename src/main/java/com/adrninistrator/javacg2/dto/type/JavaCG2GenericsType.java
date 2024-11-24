package com.adrninistrator.javacg2.dto.type;

/**
 * @author adrninistrator
 * @date 2024/10/24
 * @description: 泛型相关的类型信息
 */
public class JavaCG2GenericsType extends JavaCG2Type {

    // 泛型类型变量名称
    private String typeVariablesName;

    // 通配符
    private String wildcard;

    // 通配符引用的类型，如extends xxx，super xxx
    private String referenceType;

    public String getTypeVariablesName() {
        return typeVariablesName;
    }

    public void setTypeVariablesName(String typeVariablesName) {
        this.typeVariablesName = typeVariablesName;
    }

    public String getWildcard() {
        return wildcard;
    }

    public void setWildcard(String wildcard) {
        this.wildcard = wildcard;
    }

    public String getReferenceType() {
        return referenceType;
    }

    public void setReferenceType(String referenceType) {
        this.referenceType = referenceType;
    }

    @Override
    public String toString() {
        return "JavaCG2GenericsType{" +
                "typeVariablesName='" + typeVariablesName + '\'' +
                ", wildcard='" + wildcard + '\'' +
                ", referenceType='" + referenceType + '\'' +
                ", type='" + type + '\'' +
                ", arrayDimensions=" + arrayDimensions +
                '}';
    }
}
