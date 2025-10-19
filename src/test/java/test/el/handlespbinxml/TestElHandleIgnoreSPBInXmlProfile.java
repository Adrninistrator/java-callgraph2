package test.el.handlespbinxml;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import org.junit.Test;
import test.base.TestElBase;

/**
 * @author adrninistrator
 * @date 2025/10/18
 * @description:
 */
public class TestElHandleIgnoreSPBInXmlProfile extends TestElBase {
    @Override
    protected JavaCG2ElConfigEnum chooseElConfigEnum() {
        return JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML;
    }

    @Override
    protected String chooseElText() {
        return JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName() + " != 'dev'";
    }

    @Override
    protected String chooseTitle() {
        return "XML中的Spring Bean判断profile";
    }

    @Override
    protected String chooseDesc() {
        return "在处理XML文件中的Spring Bean时，判断profile是否等于指定关键字，仅处理匹配的Spring Bean";
    }

    @Test
    public void test() {
        run(null);
    }
}
