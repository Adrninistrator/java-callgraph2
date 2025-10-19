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
public class TestElParseIgnoreMethodClassName extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD;
    }

    @Override
    protected String chooseElText() {
        return "!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.')";
    }

    @Override
    protected String chooseTitle() {
        return "解析方法判断类名";
    }

    @Override
    protected String chooseDesc() {
        return "在解析方法时，判断类名是否以指定关键字开头，仅处理匹配的方法";
    }

    @Test
    public void test() {
        run(null);
    }
}
