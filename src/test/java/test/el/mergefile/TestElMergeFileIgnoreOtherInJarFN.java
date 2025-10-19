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
public class TestElMergeFileIgnoreOtherInJarFN extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR;
    }

    @Override
    protected String chooseElText() {
        return "!string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", '.MF')";
    }

    @Override
    protected String chooseTitle() {
        return "合并jar文件中的其他文件判断文件名";
    }

    @Override
    protected String chooseDesc() {
        return "在合并jar文件中的其他文件时，判断文件名是否以指定关键字结尾，仅处理匹配的文件";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_JAR);
    }
}
