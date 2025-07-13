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
 * @date 2025/3/16
 * @description:
 */
public class TestParseElFixed extends TestBase {

    // 所有表达式均配置为返回false
    @Test
    public void testElFalse() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        for (JavaCG2ElConfigEnum javaCG2ElConfigEnum : JavaCG2ElConfigEnum.values()) {
            javaCG2ConfigureWrapper.setElConfigFixedFalse(javaCG2ElConfigEnum);
        }
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    // 所有表达式均配置为返回true
    @Test
    public void testElTrue() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        for (JavaCG2ElConfigEnum javaCG2ElConfigEnum : JavaCG2ElConfigEnum.values()) {
            javaCG2ConfigureWrapper.setElConfigFixedTrue(javaCG2ElConfigEnum);
        }
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
