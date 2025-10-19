package test.el.stringcompare;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/10/4
 * @description:
 */
public class TestElParseIgnoreClassClassNameEquals extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS;
    }

    @Override
    protected boolean example4StringCompare() {
        return true;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'";
    }

    @Override
    protected String chooseTitle() {
        return "判断等于关键字";
    }

    @Override
    protected String chooseDesc() {
        return "在解析类时，判断类名是否等于指定关键字，仅处理匹配的类";
    }

    @Test
    public void test() {
        run(null);
    }
}
