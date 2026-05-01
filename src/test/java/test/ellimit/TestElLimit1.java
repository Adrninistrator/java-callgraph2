package test.ellimit;

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
 * @date 2026/4/26
 * @description:
 */
public class TestElLimit1 extends TestBase {

    @Test
    public void test() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_IGNORE_DATA_MAX_LINE_NUM, "20");
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.')"
        );

        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
