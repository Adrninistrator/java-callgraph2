package com.adrninistrator.javacg.common.enums;

import com.adrninistrator.javacg.common.JavaCGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCGOtherConfigFileUseListEnum {
    OCFULE_JAR_DIR(JavaCGConstants.DIR_CONFIG + "/jar_dir.properties", "指定需要处理的jar包，或保存class、jar文件的目录"),
    ;

    private final String fileName;
    private final String desc;

    JavaCGOtherConfigFileUseListEnum(String fileName, String desc) {
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
