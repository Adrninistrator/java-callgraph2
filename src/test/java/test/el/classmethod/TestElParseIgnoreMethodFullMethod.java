package test.el.classmethod;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/9/28
 * @description:
 */
public class TestElParseIgnoreMethodFullMethod extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD.getVariableName() + " != 'test.callgraph.implement.AbstractClass1:test()'";
    }

    @Override
    protected String chooseTitle() {
        return "解析方法判断完整方法";
    }

    @Override
    protected String chooseDesc() {
        return "在解析方法时，判断完整方法是否等于指定关键字，仅处理匹配的方法";
    }

    @Test
    public void test() {
        run(null);
    }
}
