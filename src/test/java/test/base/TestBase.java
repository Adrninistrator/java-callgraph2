package test.base;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/1
 * @description: 测试基类
 */
public abstract class TestBase {

    private static final Logger logger = LoggerFactory.getLogger(TestBase.class);

    public static final String JAR_OUTPUT_DIR = "jar_output_dir";
    public static final String JAR_OUTPUT_JAR = "build/jar_output.jar";
    public static final String JAR_OUTPUT_WAR = "build/jar_output.war";
    public static final String OUT_TEST_DIR = "out/test";

    /**
     * 获取 java-all-call-graph 项目中的 test.jar 文件路径
     *
     * @return
     */
    protected String getJarInJacgDirPath(String jarPath) {
        File parentDir = new File("..");
        String jacgDirPath = JavaCG2FileUtil.getCanonicalPath(parentDir) + "/java-all-call-graph/";
        String jacgJarPath1 = jacgDirPath + jarPath;
        if (new File(jacgJarPath1).exists()) {
            return jacgJarPath1;
        }
        String jacgJarPath2 = jacgDirPath + "java-all-call-graph/" + jarPath;
        if (new File(jacgJarPath2).exists()) {
            return jacgJarPath2;
        }

        logger.error("在 java-all-call-graph 项目中未找到指定的jar文件 {} {} {}", jarPath, jacgJarPath1, jacgJarPath2);
        throw new JavaCG2RuntimeException("在 java-all-call-graph 项目中未找到指定的jar文件 " + jarPath);
    }

    protected JavaCG2ConfigureWrapper genJavaCG2ConfigureWrapper4JACGTest() {
        return genJavaCG2ConfigureWrapper(true);
    }

    protected JavaCG2ConfigureWrapper genJavaCG2ConfigureWrapper4JavaCg2(String... inputFiles) {
        return genJavaCG2ConfigureWrapper(false, inputFiles);
    }

    protected JavaCG2ConfigureWrapper genJavaCG2ConfigureWrapper(boolean findInJacgDir, String... inputFiles) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        List<String> jarList = genJarList(findInJacgDir, inputFiles);
        logger.info("需要解析的jar或目录 {}", StringUtils.join(jarList, " "));
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, jarList);

        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, Boolean.FALSE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_LOG_METHOD_SPEND_TIME, Boolean.TRUE.toString());
//        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH,"");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT, ".md");
        return javaCG2ConfigureWrapper;
    }

    private List<String> genJarList(boolean findInJacgDir, String... inputFiles) {
        List<String> inputFileList = new ArrayList<>();
        if (ArrayUtils.isEmpty(inputFiles)) {
            inputFileList.add("/build/test.jar");
        } else {
            inputFileList.addAll(Arrays.asList(inputFiles));
        }
        if (!findInJacgDir) {
            return inputFileList;
        }

        List<String> jarInJacgDirPathList = new ArrayList<>(inputFileList.size());
        for (String inputFile : inputFileList) {
            String jarInJacgDirPath = getJarInJacgDirPath(inputFile);
            jarInJacgDirPathList.add(jarInJacgDirPath);
        }
        return jarInJacgDirPathList;
    }

    protected void run(boolean success, boolean findInJacgDir, String... inputFiles) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper(findInJacgDir, inputFiles);
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        boolean result = javaCG2Entry.run();
        Assert.assertEquals(success, result);
    }
}
