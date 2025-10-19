package test.el.fixed;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/9/23
 * @description:
 */
public class TestElFixedFalse extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_DIR;
    }

    @Override
    protected String chooseElText() {
        return "false";
    }

    @Override
    protected String chooseTitle() {
        return "合并jar文件时全部不忽略";
    }

    @Override
    protected String chooseDesc() {
        return "在合并目录中的jar文件时，全部不忽略";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_DIR);
    }
}
