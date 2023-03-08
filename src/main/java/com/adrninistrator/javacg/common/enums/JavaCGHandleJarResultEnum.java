package com.adrninistrator.javacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/2/14
 * @description: 处理jar包结果枚举
 */
public enum JavaCGHandleJarResultEnum {
    HJRE_FAIL("FAIL", "获取jar包信息失败"),
    HJRE_FIRST("FIRST", "获取jar包信息成功，第一次处理该jar包"),
    HJRE_NOT_FIRST("NOT_FIRST", "获取jar包信息成功，不是第一次处理该jar包"),
    ;

    private final String result;

    private final String desc;

    JavaCGHandleJarResultEnum(String result, String desc) {
        this.result = result;
        this.desc = desc;
    }

    @Override
    public String toString() {
        return result + "-" + desc;
    }
}
