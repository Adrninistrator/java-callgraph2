package com.adrninistrator.javacg2.conf.writer;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description:
 */
public class JavaCG2ConfigWriter extends BaseConfigWriter {

    public JavaCG2ConfigWriter(String rootDirPath) {
        super(rootDirPath);
    }

    // 增加自定义的el函数说明
    @Override
    protected String chooseElExampleText() {
        return "例如在 " + JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS.getKey() + " 配置文件中指定以下内容" +
                JavaCG2Constants.NEW_LINE_WINDOWS + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " == 'a.b' " +
                "&& string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + ", 'Test')" +
                JavaCG2Constants.NEW_LINE_WINDOWS + "代表在解析类时，假如类的包名等于 'a.b'，且类的简单类名以 'Test' 结尾，则跳过解析对应的类";
    }

    // todo 在生成的_javacg2_all_config.md文件中，打印el配置
    @Override
    protected String chooseElDebugModeText() {
        return "将配置文件 " + JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE.getFileName() + " 的 " + JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE.getKey() + " 参数设置为 " +
                Boolean.TRUE + " 可以使表达式执行时开启调试模式，会在应用日志中输出表达式执行时的详细信息";
    }
}
