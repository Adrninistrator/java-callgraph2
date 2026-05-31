package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeRawActualEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2ConfigException;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description: 配置参数包装类
 */
public class JavaCG2ConfigureWrapper extends BaseConfigureWrapper {

    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ConfigureWrapper.class);

    public JavaCG2ConfigureWrapper() {
    }

    public JavaCG2ConfigureWrapper(boolean onlyUseConfigInJavaCode, String inputRootPath) {
        super(onlyUseConfigInJavaCode, inputRootPath);
    }

    public JavaCG2ConfigureWrapper(boolean onlyUseConfigInJavaCode) {
        super(onlyUseConfigInJavaCode, null);
    }

    @Override
    protected Object customGenMainConfigValue(MainConfigInterface mainConfig, String strValue) throws JavaCG2ConfigException {
        String mainConfigInfo = mainConfig.getFileName() + " " + mainConfig.getConfigPrintInfo();

        if (JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL == mainConfig) {
            return handleCalleeNewRawActual(strValue, mainConfigInfo);
        }

        if (JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL == mainConfig) {
            return handleCalleeSpringBeanRawActual(strValue, mainConfigInfo);
        }

        if (JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT == mainConfig) {
            return handleOutputFileExt(strValue, mainConfigInfo);
        }

        return null;
    }

    // 处理解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型
    private String handleCalleeNewRawActual(String strValue, String mainConfigInfo) throws JavaCG2ConfigException {
        JavaCG2CalleeRawActualEnum calleeNewRawActual = JavaCG2CalleeRawActualEnum.getFromType(strValue);
        if (calleeNewRawActual == null) {
            String errorMsg = "参数值非法，允许使用的值为 " + JavaCG2CalleeRawActualEnum.getAllInfoOneLine() + " " + mainConfigInfo + " " + strValue;
            logger.error(errorMsg);
            throw new JavaCG2ConfigException(errorMsg);
        }
        return strValue;
    }

    // 处理解析方法调用时，被调用对象为Spring Bean，类型使用原始类型还是实际类型（支持字段注入）
    private String handleCalleeSpringBeanRawActual(String strValue, String mainConfigInfo) throws JavaCG2ConfigException {
        JavaCG2CalleeRawActualEnum calleeRawActual = JavaCG2CalleeRawActualEnum.getFromType(strValue);
        if (calleeRawActual == null) {
            String errorMsg = "参数值非法，允许使用的值为 " + JavaCG2CalleeRawActualEnum.getAllInfoOneLine() + " " + mainConfigInfo + " " + strValue;
            logger.error(errorMsg);
            throw new JavaCG2ConfigException(errorMsg);
        }
        return strValue;
    }

    // 处理生成文件后缀名
    private String handleOutputFileExt(String strValue, String mainConfigInfo) throws JavaCG2ConfigException {
        if (StringUtils.isNotBlank(strValue) && !StringUtils.startsWith(strValue, JavaCG2Constants.FLAG_DOT)) {
            String errorMsg = "配置参数需要以 " + JavaCG2Constants.FLAG_DOT + " 开头 " + mainConfigInfo + " " + strValue;
            logger.error(errorMsg);
            throw new JavaCG2ConfigException(errorMsg);
        }
        return strValue;
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
