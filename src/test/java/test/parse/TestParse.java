package test.parse;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description:
 */
public class TestParse extends TestBase {

    @Test
    public void testParse() {
        run(true);
    }

    @Test
    public void testParseNoMCTypeValue() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    @Test
    public void testElFalse() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper();
        for (JavaCG2ElConfigEnum javaCG2ElConfigEnum : JavaCG2ElConfigEnum.values()) {
            javaCG2ConfigureWrapper.setElConfigFixedFalse(javaCG2ElConfigEnum);
        }
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
