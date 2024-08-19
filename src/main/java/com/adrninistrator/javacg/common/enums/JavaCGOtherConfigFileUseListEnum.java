package com.adrninistrator.javacg.common.enums;

import com.adrninistrator.javacg.common.JavaCGConstants;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCGOtherConfigFileUseListEnum {
    OCFULE_JAR_DIR(JavaCGConstants.DIR_CONFIG + "/jar_dir.properties",
            "(作用) 指定需要处理的jar包路径，或保存class、jar文件的目录路径（每行代表一个jar包或目录路径，支持多行）"
                    + JavaCGConstants.NEW_LINE + "(格式) 路径中的分隔符使用/或\\均可，目录最后指定或不指定分隔符均可"
                    + JavaCGConstants.NEW_LINE + "(示例) build/libs/"
                    + JavaCGConstants.NEW_LINE + "(示例) build/libs/test.jar"
                    + JavaCGConstants.NEW_LINE + "(示例) D:/test/build/libs/test.jar"
    ),
    OCFULE_CODE_PARSER_ONLY_4SHOW("代码解析扩展类名（仅用于显示）",
            "对代码进行解析的扩展类完整类名"
    ),
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
