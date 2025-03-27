package test.config;

import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeRawActualEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/3/24
 * @description:
 */
public class TestCalleeRawActual extends TestBase {

    @Test
    public void test() {
        for (JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnumNew : JavaCG2CalleeRawActualEnum.values()) {
            for (JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnumSpringBean : JavaCG2CalleeRawActualEnum.values()) {
                doTest(javaCG2CalleeRawActualEnumNew, javaCG2CalleeRawActualEnumSpringBean);
            }
        }
    }

    @Test
    public void test2() {
        doTest(JavaCG2CalleeRawActualEnum.CRAE_ONLY_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_ONLY_RAW);
    }

    private void doTest(JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnumNew, JavaCG2CalleeRawActualEnum javaCG2CalleeRawActualEnumSpringBean) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper();
        String outRootPath = String.format("build/callee_raw_actual@new_%s_spb_%s", javaCG2CalleeRawActualEnumNew.getType(), javaCG2CalleeRawActualEnumSpringBean.getType());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, outRootPath);
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL, javaCG2CalleeRawActualEnumNew.getType());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL, javaCG2CalleeRawActualEnumSpringBean.getType());
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
    }
}
