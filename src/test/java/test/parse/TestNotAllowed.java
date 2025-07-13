package test.parse;

import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/2/3
 * @description:
 */
public class TestNotAllowed extends TestBase {

    @Test
    public void testSingleClass() {
        run(false, false, "out/test/classes/test/base/TestBase.class");
    }

    @Test
    public void testMultiClass() {
        run(false, false,
                "out/test/classes/test/base/TestBase.class",
                "out/test/classes/test/simple/TestSimple.class"
        );
    }
}
