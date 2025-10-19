package test.el.stringcompare;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/10/3
 * @description:
 */
public class TestElParseIgnoreClassClassNameOr extends TestElBase {
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
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == 'test.callgraph.annotation.CallMethodWithAnnotation' || " +
                JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == 'test.callgraph.annotation.MethodWithAnnotation'";
    }

    @Override
    protected String chooseTitle() {
        return "判断多个条件使用或";
    }

    @Override
    protected String chooseDesc() {
        return "在解析类时，多个条件使用或运算，判断类名是否等于指定多个关键字中的任意一个，忽略匹配的类";
    }

    @Test
    public void test() {
        run(null);
    }
}
