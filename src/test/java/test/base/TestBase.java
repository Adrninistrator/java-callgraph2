package test.base;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.Assert;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2025/2/1
 * @description: 测试基类
 */
public abstract class TestBase {

    private static final Logger logger = LoggerFactory.getLogger(TestBase.class);
    protected String jacgTestListPath;

    @Before
    public void initTestBase() {
        jacgTestListPath = getJacgTestLibPath();
    }

    /**
     * 获取 java-all-call-graph 项目中的 test.jar 文件路径
     *
     * @return
     */
    protected String getJacgTestLibPath() {
        File parentDir = new File("..");
        String jacgDirPath = JavaCG2FileUtil.getCanonicalPath(parentDir) + File.separator + "java-all-call-graph";
        String jacgTestLibPath1 = jacgDirPath + "/build/libs/test.jar";
        if (new File(jacgTestLibPath1).exists()) {
            return jacgTestLibPath1;
        }
        String jacgTestLibPath2 = jacgDirPath + "/java-all-call-graph/build/libs/test.jar";
        if (new File(jacgTestLibPath2).exists()) {
            return jacgTestLibPath2;
        }

        throw new JavaCG2RuntimeException("在目录 " + jacgTestLibPath1 + " " + jacgTestLibPath2 + " 中未找到 java-all-call-graph 项目的 test.jar");
    }

    protected JavaCG2ConfigureWrapper genJavaCG2ConfigureWrapper(String... inputFiles) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        if (ArrayUtils.isEmpty(inputFiles)) {
            javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, jacgTestListPath);
        } else {
            javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, inputFiles);
        }
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, Boolean.FALSE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_LOG_METHOD_SPEND_TIME, Boolean.TRUE.toString());
//        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH,"");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT, ".md");
        return javaCG2ConfigureWrapper;
    }

    protected void run(boolean success, String... inputFiles) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper(inputFiles);
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        if (success) {
            Assert.assertTrue(javaCG2Entry.run());
        } else {
            Exception e = Assert.assertThrows(Exception.class, javaCG2Entry::run);
            logger.error("error ", e);
        }
    }
}
