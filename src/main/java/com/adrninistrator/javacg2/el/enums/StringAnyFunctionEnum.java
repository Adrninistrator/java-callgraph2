package com.adrninistrator.javacg2.el.enums;

/**
 * @author adrninistrator
 * @date 2025/2/10
 * @description:
 */
public enum StringAnyFunctionEnum {
    CONTAINS_ANY("string.containsAny"),
    ENDS_WITH_ANY("string.endsWithAny"),
    EQUALS_ANY("string.equalsAny"),
    STARTS_WITH_ANY("string.startsWithAny"),
    ;

    private final String name;

    StringAnyFunctionEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
