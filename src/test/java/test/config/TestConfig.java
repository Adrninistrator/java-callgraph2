package test.config;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public class TestConfig {

    private final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();

    @Before
    public void init() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, Collections.singletonList("build/libs/java-callgraph2-2.0.8.jar"));
    }

    @Test
    public void testDefault() {
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, Boolean.FALSE.toString());
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT, ".md");
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    @Test
    public void testDebugPrintOn() {
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    @Test
    public void testDebugPrintInFile() {
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    @Test
    public void testParseMethodCallTypeValueOff() {
        javaCG2ConfigureWrapper.setConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    @Test
    public void testMultiTimes() {
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
        Assert.assertFalse(javaCG2Entry.run());
    }
}
