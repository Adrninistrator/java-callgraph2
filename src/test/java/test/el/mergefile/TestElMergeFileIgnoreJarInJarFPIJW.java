package test.el.mergefile;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/9/24
 * @description:
 */
public class TestElMergeFileIgnoreJarInJarFPIJW extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR;
    }

    @Override
    protected String chooseElText() {
        return "string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR.getVariableName() + ", 'lib/')";
    }

    @Override
    protected String chooseTitle() {
        return "合并jar文件中的jar文件判断文件相对路径";
    }

    @Override
    protected String chooseDesc() {
        return "在合并jar文件中的jar文件时，判断文件相对路径是否以指定关键字开头，忽略匹配的文件";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_JAR);
    }
}
