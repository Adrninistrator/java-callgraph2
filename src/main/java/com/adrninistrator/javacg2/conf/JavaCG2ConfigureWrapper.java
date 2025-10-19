package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;

import java.io.IOException;

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
        // 打印主要的配置文件
        printMainConfigInfo(markdownWriter, JavaCG2ConfigKeyEnum.values(), printAllConfigInfo);

        // 打印Set格式的其他配置参数
        printOtherSetConfigInfo(markdownWriter, JavaCG2OtherConfigFileUseSetEnum.values(), printAllConfigInfo);

        // 打印List格式的其他配置参数
        printOtherListConfigInfo(markdownWriter, JavaCG2OtherConfigFileUseListEnum.values(), printAllConfigInfo);

        // 打印表达式配置参数
        printElConfigInfo(markdownWriter, JavaCG2ElConfigEnum.values(), printAllConfigInfo);
    }

    @Override
    protected String getMainConfigSCNFromFile(String mainConfigFile) {
        return JavaCG2ConfigKeyEnum.class.getSimpleName();
    }

    @Override
    public OtherConfigInterface[] chooseOtherConfigFileUseSetEnums() {
        return JavaCG2OtherConfigFileUseSetEnum.values();
    }

    @Override
    public OtherConfigInterface[] chooseOtherConfigFileUseListEnums() {
        return JavaCG2OtherConfigFileUseListEnum.values();
    }

    @Override
    public ElConfigInterface[] chooseElConfigEnums() {
        return JavaCG2ElConfigEnum.values();
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
