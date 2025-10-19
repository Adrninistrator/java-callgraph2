package com.adrninistrator.javacg2.conf.writer;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;

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
        return JavaCG2Constants.NEW_LINE_WINDOWS + "字符串比较的表达式示例可参考 " + JavaCG2DirEnum.IDE_EL_EXAMPLE.getDirName() + "/" + JavaCG2Constants.EL_STRING_COMPARE_MD_FILE_NAME +
                JavaCG2Constants.NEW_LINE_WINDOWS +
                JavaCG2Constants.NEW_LINE_WINDOWS + "不同场景的表达式示例可参考 " + JavaCG2DirEnum.IDE_EL_EXAMPLE.getDirName() + " 目录中子目录的对应文件"
                ;
    }

    @Override
    protected String chooseElDebugModeText() {
        return "将配置文件 " + JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE.getFileName() + " 的 " + JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE.getKey() + " 参数设置为 " +
                Boolean.TRUE + " 可以使表达式执行时开启调试模式，会在应用日志中输出表达式执行时的详细信息";
    }
}
