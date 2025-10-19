package test.base;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;

/**
 * @author adrninistrator
 * @date 2025/9/23
 * @description: 表达式测试基类
 */
public abstract class TestElBase extends TestBase {

    /**
     * 选择需要测试的EL表达式枚举常量
     *
     * @return
     */
    protected abstract JavaCG2ElConfigEnum chooseElConfigEnum();

    /**
     * 选择需要使用的EL表达式文本
     *
     * @return
     */
    protected abstract String chooseElText();

    /**
     * 选择当前测试类显示的标题
     *
     * @return
     */
    protected abstract String chooseTitle();

    /**
     * 选择当前测试类的描述
     *
     * @return
     */
    protected abstract String chooseDesc();

    /**
     * 返回当前示例是否为字符串比较
     *
     * @return
     */
    protected boolean example4StringCompare() {
        return false;
    }

    /**
     * 设置EL表达式配置参数
     */
    protected void setConfig4ElText(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper) {
        JavaCG2ElConfigEnum elConfigEnum = chooseElConfigEnum();
        String elText = chooseElText();
        javaCG2ConfigureWrapper.setElConfigText(elConfigEnum, elText);
    }

    protected void run(String jarDirPath) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;
        if (StringUtils.isNotBlank(jarDirPath)) {
            javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JavaCg2(jarDirPath);
        } else {
            javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        }
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE, Boolean.TRUE.toString());
        setConfig4ElText(javaCG2ConfigureWrapper);
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
