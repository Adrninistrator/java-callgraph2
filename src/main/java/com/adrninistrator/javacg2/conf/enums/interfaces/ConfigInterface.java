package com.adrninistrator.javacg2.conf.enums.interfaces;

import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2022/11/14
 * @description: 配置枚举继承的接口
 */
public interface ConfigInterface {

    // 获取枚举常量名称
    String getEnumConstantName();

    // 获取key
    String getKey();

    // 获取描述
    String[] getDescriptions();

    // 获取配置用于打印的信息
    String getConfigPrintInfo();

    /**
     * 生成配置参数的使用说明
     *
     * @return
     */
    String genConfigUsage();

    /**
     * 生成配置参数的使用说明
     *
     * @param configWrapperClass
     * @return
     */
    default String doGenConfigUsage(Class<?> configWrapperClass) {
        String objName = JavaCG2ClassMethodUtil.getFirstLetterLowerClassName(configWrapperClass.getSimpleName());
        String enumSimpleClassName = this.getClass().getSimpleName();
        String enumConstantName = getEnumConstantName();
        String configFileName = null;
        String configFileKeyName = null;
        String configMethodName = null;
        if (this instanceof ElConfigInterface) {
            ElConfigInterface elConfigInterface = (ElConfigInterface) this;
            configFileName = elConfigInterface.getKey();
            configMethodName = "setElConfigText";
        }
        if (this instanceof MainConfigInterface) {
            MainConfigInterface mainConfigInterface = (MainConfigInterface) this;
            configFileName = mainConfigInterface.getFileName();
            configFileKeyName = mainConfigInterface.getKey();
            configMethodName = "setMainConfig";
        }
        if (this instanceof OtherConfigInterface) {
            OtherConfigInterface otherConfigInterface = (OtherConfigInterface) this;
            configFileName = otherConfigInterface.getKey();
            configMethodName = otherConfigInterface.isSetOrList() ? "setOtherConfigSet" : "setOtherConfigList";
        }
        if (configFileName == null) {
            return "不支持的配置参数类型 " + this.getClass().getName();
        }
        StringBuilder stringBuilder = new StringBuilder("当前参数可通过以下方式进行配置 ");
        stringBuilder.append("1. 通过配置文件进行配置: ").append(configFileName);
        if (configFileKeyName != null) {
            stringBuilder.append(" 配置参数名称为： ").append(configFileKeyName);
        }
        stringBuilder.append(" 2. 通过代码进行配置: ").append(objName)
                .append(".").append(configMethodName).append("(").append(enumSimpleClassName)
                .append(".").append(enumConstantName).append(", ...);");
        return stringBuilder.toString();
    }
}
