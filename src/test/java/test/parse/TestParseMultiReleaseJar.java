package test.parse;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.bouncycastle.LICENSE;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/7/29
 * @description:
 */
public class TestParseMultiReleaseJar extends TestBase {
    private static final Logger logger = LoggerFactory.getLogger(TestParseMultiReleaseJar.class);

    private String bcprovJarFilePath;

    @Before
    public void init() {
        bcprovJarFilePath = JavaCG2FileUtil.getJarFilePathOfClass(LICENSE.class);
        logger.info("bcprovJarFilePath {}", bcprovJarFilePath);
    }

    @Test
    public void testMultiReleaseJarVersion8() {
        doTest(8);
    }

    @Test
    public void testMultiReleaseJarVersion9() {
        doTest(9);
    }

    @Test
    public void testMultiReleaseJarVersion11() {
        doTest(11);
    }

    @Test
    public void testMultiReleaseJarVersion15() {
        doTest(15);
    }

    @Test
    public void testMultiReleaseJarVersion21() {
        doTest(21);
    }

    private void doTest(int version) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JavaCg2(bcprovJarFilePath);
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_JDK_RUNTIME_MAJOR_VERSION, String.valueOf(version));
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, bcprovJarFilePath + "@version_" + version);
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }
}
