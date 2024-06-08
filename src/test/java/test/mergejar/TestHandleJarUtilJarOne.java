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
public class TestHandleJarUtilJarOne {

    @Test
    public void test() {
        Map<String, JarInfo> jarInfoMap = new HashMap<>();

        System.out.println(JavaCGJarUtil.handleJar(Collections.singletonList(
                "E:/desktop/test-jar/disruptor-3.3.7.jar"
        ), jarInfoMap, new HashSet<>(), new HashSet<>()));
    }
}
