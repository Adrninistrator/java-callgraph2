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
public class TestElMergeFileIgnoreClassInDirFN extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR;
    }

    @Override
    protected String chooseElText() {
        return "string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", 'TestEl')";
    }

    @Override
    protected String chooseTitle() {
        return "合并目录中的class文件判断文件名";
    }

    @Override
    protected String chooseDesc() {
        return "在合并目录中的class文件时，判断文件名是否以指定关键字开头，忽略匹配的文件";
    }

    @Test
    public void test() {
        run(OUT_TEST_DIR);
    }
}
