package com.adrninistrator.javacg2.conf.enums;

import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCG2OtherConfigFileUseListEnum implements OtherConfigInterface {
    OCFULE_JAR_DIR(JavaCG2DirEnum.IDE_CONFIG.getDirName() + "/jar_dir.properties",
            new String[]{"(作用) 指定需要解析的jar、war文件路径，或保存class、jar、war文件的目录路径（每行代表一条记录，支持多行）",
                    "(格式) 路径中的分隔符支持使用/或\\，目录最后指定或不指定分隔符均可",
                    "(示例) build/",
                    "(示例) build/test.jar",
                    "(示例) D:/test/build/test.jar"},
            null
    ),
    OCFULE_CODE_PARSER_ONLY_4SHOW("代码解析扩展类名（仅用于显示）",
            new String[]{"对代码进行解析的扩展类完整类名"},
            null
    ),
    ;

    // 参数配置文件名
    private final String fileName;
    // 参数配置描述
    private final String[] descriptions;
    // 默认值
    private final String[] defaultValues;

    JavaCG2OtherConfigFileUseListEnum(String fileName, String[] descriptions, String[] defaultValues) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.defaultValues = defaultValues;
    }

    @Override
    public String getEnumConstantsName() {
        return name();
    }

    @Override
    public String getKey() {
        return fileName;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String[] getDefaultValues() {
        return defaultValues;
    }

    @Override
    public String getConfigPrintInfo() {
        return fileName + " " + JavaCG2OtherConfigFileUseListEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public boolean isSetOrList() {
        return false;
    }

    @Override
    public OtherConfigInterface getFromKey(String key) {
        for (JavaCG2OtherConfigFileUseListEnum otherConfigFileUseListEnum : JavaCG2OtherConfigFileUseListEnum.values()) {
            if (otherConfigFileUseListEnum.getKey().equals(key)) {
                return otherConfigFileUseListEnum;
            }
        }
        throw new JavaCG2RuntimeException("不存在的key " + key);
    }

    @Override
    public String toString() {
        return fileName;
    }
}
