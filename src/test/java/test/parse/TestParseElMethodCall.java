package test.parse;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
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
public class TestParseElMethodCall extends TestBase {

    // 处理方法调用时仅处理指定包下的调用类与被调用类
    @Test
    public void testParseMCEREE1() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER_EE,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.array')" +
                        " && !string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.array')"
        );
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    // 处理方法调用时仅处理指定包下的调用类与被调用类
    @Test
    public void testParseMCEREE2() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER_EE,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", 'test.callgraph')" +
                        " || !string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + ", 'test.callgraph')"
        );
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    // 处理方法调用时仅处理指定包下的调用类
    @Test
    public void testParseMCER1() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER_EE,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.array')");
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    // 处理方法调用时仅处理指定包下的被调用类
    @Test
    public void testParseMCEE1() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER_EE,
                "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + ", 'test.callgraph.array')"
        );
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
