package test.parse;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description:
 */
public class TestParse extends TestBase {

    // 正常解析
    @Test
    public void testParse() {
        run(true);
    }

    // 不解析被调用对象和参数可能的类型与值
    @Test
    public void testParseNoMCTypeValue() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.FALSE.toString());
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }

    /*
        需要先执行 gradlew gen_run_jar gen_jar_in_jar 命令生成对应jar文件
     */
    @Test
    public void testJarInJar() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper("build/jar_output_dir.jar");
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }
}
