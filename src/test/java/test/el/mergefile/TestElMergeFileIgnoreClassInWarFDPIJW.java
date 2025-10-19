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
public class TestElMergeFileIgnoreClassInWarFDPIJW extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR;
    }

    @Override
    protected String chooseElText() {
        return "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + ", 'WEB-INF/classes/test/')";
    }

    @Override
    protected String chooseTitle() {
        return "合并war文件中的class文件判断文件所在目录相对路径";
    }

    @Override
    protected String chooseDesc() {
        return "在合并war文件中的class文件时，判断文件所在目录相对路径（相对根目录）是否以指定关键字开头，仅处理匹配的文件";
    }

    @Test
    public void test() {
        run(JAR_OUTPUT_WAR);
    }
}
