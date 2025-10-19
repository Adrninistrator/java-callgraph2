package test.el.methodcalltypevalue;

import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/10/10
 * @description:
 */
public class TestElParseIgnoreMethodCallTypeValueErPackageName extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER;
    }

    @Override
    protected String chooseElText() {
        return CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'";
    }

    @Override
    protected String chooseTitle() {
        return "方法调用类型与值判断调用类包名";
    }

    @Override
    protected String chooseDesc() {
        return "在解析方法调用被调用对象和参数可能的类型与值时，判断调用类包名是否等于指定关键字，仅处理匹配的调用方法";
    }

    @Test
    public void test() {
        run(null);
    }
}
