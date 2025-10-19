package test.el.classmethod;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/9/24
 * @description:
 */
public class TestElParseIgnoreClassSimpleClassName extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'";
    }

    @Override
    protected String chooseTitle() {
        return "解析类判断简单类名";
    }

    @Override
    protected String chooseDesc() {
        return "在解析类时，判断简单类名是否等于指定关键字，仅处理匹配的类";
    }

    @Test
    public void test() {
        run(null);
    }
}
