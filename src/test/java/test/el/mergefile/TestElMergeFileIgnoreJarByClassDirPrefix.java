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
public class TestElMergeFileIgnoreJarByClassDirPrefix extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX;
    }

    @Override
    protected String chooseElText() {
        return "!include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "2, 'com/adrninistrator') && "
                + "!include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "3, 'org/apache/bcel')";
    }

    @Override
    protected String chooseTitle() {
        return "合并jar文件判断所有class文件所在指定层级的目录路径";
    }

    @Override
    protected String chooseDesc() {
        return "在合并jar文件时，判断所有class文件所在指定层级的目录路径是否包括指定的路径，仅处理class文件所在指定层级的目录路径包括特定路径的jar文件";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_DIR);
    }
}
