package com.adrninistrator.javacg2.conf.enums;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCG2OtherConfigFileUseSetEnum implements OtherConfigInterface {
    OCFUSE_FR_EQ_CONVERSION_METHOD(JavaCG2DirEnum.IDE_CONFIG.getDirName() + "/fr_eq_conversion_method.properties",
            new String[]{"(作用) 在分析dto的字段之间通过get/set方法的关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法（每行代表一条记录，支持多行）",
                    "(内容) key指定对应的方法，包含{完整类名}:{方法名}",
                    "(内容) value指定与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号",
                    "(格式) {完整类名}:{方法名}={被调用对象或方法参数序号}"},
            JavaCG2Constants.FR_EQ_CONVERSION_METHODS
    ),
    ;

    // 参数配置文件名
    private final String fileName;
    // 参数配置描述
    private final String[] descriptions;
    // 默认值
    private final String[] defaultValues;

    JavaCG2OtherConfigFileUseSetEnum(String fileName, String[] descriptions, String[] defaultValues) {
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
        return fileName + " " + JavaCG2OtherConfigFileUseSetEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public boolean isSetOrList() {
        return true;
    }

    @Override
    public OtherConfigInterface getFromKey(String key) {
        for (JavaCG2OtherConfigFileUseSetEnum otherConfigFileUseSetEnum : JavaCG2OtherConfigFileUseSetEnum.values()) {
            if (otherConfigFileUseSetEnum.getKey().equals(key)) {
                return otherConfigFileUseSetEnum;
            }
        }
        throw new JavaCG2RuntimeException("不存在的key " + key);
    }

    @Override
    public String toString() {
        return fileName;
    }
}
