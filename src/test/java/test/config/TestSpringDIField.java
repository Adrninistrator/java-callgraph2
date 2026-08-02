package test.config;

import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 测试 Spring依赖注入字段信息文件（spring_di_field）
 *
 * 验证内容：
 * 1. 该文件不依赖符号执行开关 parse.method.call.type.value，开关 true / false 时生成的内容应一致
 *    （这是本次解耦符号执行开关改动的核心断言）
 * 2. 每行4列tab分隔：完整类名、注入的Spring Bean字段定义类型、字段名称、字段实际类型
 *
 * 前置条件：需要先在 java-all-call-graph 项目执行 gradlew test_gen_jar 命令生成对应jar文件
 *
 * @author adrninistrator
 * @date 2026/6/19
 */
public class TestSpringDIField extends TestBase {

    /**
     * 验证 Spring依赖注入字段信息文件在符号执行开关 true / false 下均能生成，且内容一致
     */
    @Test
    public void testSpringDIFieldDecoupledFromSymbolicExecution() {
        // 开关为 true 时生成文件内容
        List<String> contentOn = runAndReadSpringDIField(Boolean.TRUE.toString(), "build/spring_di_field@type_value_on");
        // 开关为 false 时生成文件内容
        List<String> contentOff = runAndReadSpringDIField(Boolean.FALSE.toString(), "build/spring_di_field@type_value_off");

        // 1. 文件非空：被解析的 jacg 测试jar中包含 @Autowired / @Resource / @Inject 字段，应能解析出Spring依赖注入字段
        Assert.assertFalse("spring_di_field 文件开关为true时应非空", contentOn.isEmpty());
        Assert.assertFalse("spring_di_field 文件开关为false时应非空", contentOff.isEmpty());

        // 2. 结构校验：每行必须为4列tab分隔，且每列均非空
        assert4Columns(contentOn);
        assert4Columns(contentOff);

        // 3. 两种开关下内容完全一致（核心断言：Spring DI字段解析已解耦符号执行开关）
        Assert.assertEquals("spring_di_field 文件在符号执行开关 true / false 下内容应一致",
                sorted(contentOn), sorted(contentOff));
    }

    // 使用指定符号执行开关运行，并读取生成的 spring_di_field 文件内容
    private List<String> runAndReadSpringDIField(String parseMethodCallTypeValue, String outRootPath) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, parseMethodCallTypeValue);
        // 指定独立的输出目录，避免两次运行互相覆盖
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, outRootPath);

        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());

        JavaCG2OutputInfo javaCG2OutputInfo = javaCG2Entry.getJavaCG2InputAndOutput().getJavaCG2OutputInfo();
        String springDIFieldFilePath = javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_SPRING_DI_FIELD);
        Assert.assertTrue("spring_di_field 文件应存在: " + springDIFieldFilePath, new File(springDIFieldFilePath).exists());

        return JavaCG2FileUtil.readFile2List(springDIFieldFilePath);
    }

    // 校验每行为4列tab分隔，且每列均非空
    private void assert4Columns(List<String> lines) {
        for (String line : lines) {
            String[] columns = line.split("\t", -1);
            Assert.assertEquals("spring_di_field 文件每行必须为4列: " + line, 4, columns.length);
            for (int i = 0; i < columns.length; i++) {
                Assert.assertFalse("spring_di_field 文件第" + (i + 1) + "列不能为空: " + line, columns[i].isEmpty());
            }
        }
    }

    private List<String> sorted(List<String> list) {
        List<String> copy = new ArrayList<>(list);
        Collections.sort(copy);
        return copy;
    }
}
