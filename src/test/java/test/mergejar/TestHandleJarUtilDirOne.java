package test.mergejar;

import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.util.JavaCGJarUtil;
import org.junit.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class TestHandleJarUtilDirOne {

    @Test
    public void test() {
        Map<String, JarInfo> jarInfoMap = new HashMap<>();

        System.out.println(JavaCGJarUtil.handleJar(Collections.singletonList(
                "E:/desktop/test-dir/out"
        ), jarInfoMap, new HashSet<>(), new HashSet<>()));
    }
}
