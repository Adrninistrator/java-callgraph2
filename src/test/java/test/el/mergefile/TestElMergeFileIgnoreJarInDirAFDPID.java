package test.el.mergefile;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/9/23
 * @description:
 */
public class TestElMergeFileIgnoreJarInDirAFDPID extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_DIR;
    }

    @Override
    protected String chooseElText() {
        return "string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName() + ", '/lib')";
    }

    @Override
    protected String chooseTitle() {
        return "合并目录中的jar文件判断文件所在目录绝对路径";
    }

    @Override
    protected String chooseDesc() {
        return "在合并目录中的jar文件时，判断文件所在目录绝对路径是否以指定关键字结尾，忽略匹配的文件";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_DIR);
    }
}
