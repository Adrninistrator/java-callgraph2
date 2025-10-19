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
public class TestElMergeFileIgnoreWarInDirFN extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_WAR_IN_DIR;
    }

    @Override
    protected String chooseElText() {
        return "string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", '.war')";
    }

    @Override
    protected String chooseTitle() {
        return "合并目录中的war文件判断文件名";
    }

    @Override
    protected String chooseDesc() {
        return "在合并目录中的war文件时，判断文件名是否以指定关键字结尾，忽略匹配的文件";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_WAR);
    }
}
