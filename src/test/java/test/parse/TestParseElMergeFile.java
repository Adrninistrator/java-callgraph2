package test.parse;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/3/16
 * @description:
 */
public class TestParseElMergeFile extends TestBase {

    /*
        忽略lib目录中的jar文件
        需要先执行 gradlew gen_run_jar 命令生成对应目录及文件
     */
    @Test
    public void testMergeFileIgnoreLibDir() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper("jar_output_dir");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_DIR,
                "string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName() + ", '/lib')"
        );
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    /*
        忽略lib目录中的jar文件
        需要先执行 gradlew gen_run_jar gen_jar_in_jar 命令生成对应jar文件
     */
    @Test
    public void testMergeFileIgnoreJarLibDir() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper("build/jar_output_dir.jar");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR,
                JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + " == 'lib'"
        );
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    /*
        忽略class文件包名不包含com.adrninistrator、text的jar文件
        需要先执行 gradlew gen_run_jar gen_jar_in_jar 命令生成对应jar文件
     */
    @Test
    public void testMergeFileIgnoreByJarClassDirPrefix1() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper("build/jar_output_dir.jar");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX,
                "!include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "2, 'com/adrninistrator')" +
                        " && !include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "1, 'test')"
        );
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR,
                "string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR.getVariableName() + ", 'test/')"
        );
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
