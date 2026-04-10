package test.elerror;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.exceptions.JavaCG2ElConfigRuntimeException;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2026/3/28
 * @description:
 */
public class TestElError extends TestBase {

    @Test
    public void test() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS, "1='1");
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        JavaCG2ElConfigRuntimeException javaCG2ElConfigRuntimeException= Assert.assertThrows(JavaCG2ElConfigRuntimeException.class, javaCG2Entry::run);
        Assert.assertEquals(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS, javaCG2ElConfigRuntimeException.getElConfig());
    }
}
