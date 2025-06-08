package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description: 配置参数包装类
 */
public class JavaCG2ConfigureWrapper extends BaseConfigureWrapper {

    public JavaCG2ConfigureWrapper() {
    }

    public JavaCG2ConfigureWrapper(boolean onlyUseConfigInJavaCode) {
        super(onlyUseConfigInJavaCode);
    }

    @Override
    protected Object customGenMainConfigValue(MainConfigInterface mainConfig, String strValue) {
        return null;
    }

    @Override
    protected void useDefaultEmptyConfig() {
        clearMainConfigs(JavaCG2ConfigKeyEnum.values());
        clearOtherConfigUseSet(JavaCG2OtherConfigFileUseSetEnum.values());
        clearOtherConfigUseList(JavaCG2OtherConfigFileUseListEnum.values());
        clearElConfigText(JavaCG2ElConfigEnum.values());
    }

    @Override
    protected Object customGetDefaultConfig(MainConfigInterface mainConfig) {
        return null;
    }

    @Override
    protected void customPrintConfigInfo(MarkdownWriter markdownWriter, boolean printAllConfigInfo) throws IOException {
        printMainConfigInfo(markdownWriter, JavaCG2ConfigKeyEnum.values(), printAllConfigInfo);

        // 打印Set格式的其他配置信息
        printOtherSetConfigInfo(markdownWriter, JavaCG2OtherConfigFileUseSetEnum.values(), printAllConfigInfo);

        // 打印List格式的其他配置信息
        List<JavaCG2OtherConfigFileUseListEnum> javaCG2OtherConfigFileUseListEnumList = new ArrayList<>();
        for (JavaCG2OtherConfigFileUseListEnum javaCG2OtherConfigFileUseListEnum : JavaCG2OtherConfigFileUseListEnum.values()) {
            if (javaCG2OtherConfigFileUseListEnum == JavaCG2OtherConfigFileUseListEnum.OCFULE_CODE_PARSER_ONLY_4SHOW) {
                continue;
            }
            javaCG2OtherConfigFileUseListEnumList.add(javaCG2OtherConfigFileUseListEnum);
        }
        JavaCG2OtherConfigFileUseListEnum[] javaCG2OtherConfigFileUseListEnums = new JavaCG2OtherConfigFileUseListEnum[javaCG2OtherConfigFileUseListEnumList.size()];
        javaCG2OtherConfigFileUseListEnums = javaCG2OtherConfigFileUseListEnumList.toArray(javaCG2OtherConfigFileUseListEnums);
        printOtherListConfigInfo(markdownWriter, javaCG2OtherConfigFileUseListEnums, printAllConfigInfo);
    }

    @Override
    protected String getMainConfigSCNFromFile(String mainConfigFile) {
        return JavaCG2ConfigKeyEnum.class.getSimpleName();
    }

    @Override
    protected OtherConfigInterface chooseOtherConfigFileUseSetEnum() {
        return JavaCG2OtherConfigFileUseSetEnum.values()[0];
    }

    @Override
    protected OtherConfigInterface chooseOtherConfigFileUseListEnum() {
        return JavaCG2OtherConfigFileUseListEnum.values()[0];
    }

    @Override
    protected String[] chooseAllowedConfigClassNames() {
        return new String[]{
                JavaCG2ConfigKeyEnum.class.getName(),
                JavaCG2OtherConfigFileUseListEnum.class.getName(),
                JavaCG2OtherConfigFileUseSetEnum.class.getName(),
                JavaCG2ElConfigEnum.class.getName()
        };
    }

    /**
     * 拷贝数据
     *
     * @return
     */
    public JavaCG2ConfigureWrapper copy() {
        return (JavaCG2ConfigureWrapper) baseCopy();
    }
}
