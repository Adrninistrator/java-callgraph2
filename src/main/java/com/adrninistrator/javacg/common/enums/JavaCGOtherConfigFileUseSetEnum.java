package com.adrninistrator.javacg.common.enums;

import com.adrninistrator.javacg.common.JavaCGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCGOtherConfigFileUseSetEnum {
    OCFUSE_PACKAGES(JavaCGConstants.DIR_CONFIG + "/packages.properties", "指定需要处理的包名"),
    ;

    private final String fileName;
    private final String desc;

    JavaCGOtherConfigFileUseSetEnum(String fileName, String desc) {
        this.fileName = fileName;
        this.desc = desc;
    }

    public String getFileName() {
        return fileName;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return fileName;
    }
}
