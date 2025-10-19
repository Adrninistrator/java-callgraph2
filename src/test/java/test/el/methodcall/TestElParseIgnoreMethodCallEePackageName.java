package test.el.methodcall;

import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/9/24
 * @description:
 */
public class TestElParseIgnoreMethodCallEePackageName extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'";
    }

    @Override
    protected String chooseTitle() {
        return "方法调用判断被调用类包名";
    }

    @Override
    protected String chooseDesc() {
        return "在解析方法调用时，判断被调用类包名是否等于指定关键字，仅处理匹配的方法调用";
    }

    @Test
    public void test() {
        run(null);
    }
}
