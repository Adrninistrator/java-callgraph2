package com.adrninistrator.javacg.enums;

/**
 * @author adrninistrator
 * @date 2022/2/14
 * @description:
 */
public enum HandleJarResultEnum {
    HJRE_FAIL("FAIL", "获取jar包信息失败"),
    HJRE_FIRST("FIRST", "获取jar包信息成功，第一次处理该jar包"),
    HJRE_NOT_FIRST("NOT_FIRST", "获取jar包信息成功，不是第一次处理该jar包"),
    ;

    private String result;

    private String desc;

    HandleJarResultEnum(String result, String desc) {
        this.result = result;
        this.desc = desc;
    }

    @Override
    public String toString() {
        return result + "-" + desc;
    }
}
