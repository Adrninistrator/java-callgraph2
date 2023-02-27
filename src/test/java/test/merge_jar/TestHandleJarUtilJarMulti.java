package test.merge_jar;

import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.util.JavaCGJarUtil;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class TestHandleJarUtilJarMulti {

    @Test
    public void test() {
        Map<String, JarInfo> jarInfoMap = new HashMap<>();

        System.out.println(JavaCGJarUtil.handleJar(Arrays.asList(
                "E:/desktop/test-jar/disruptor-3.3.7.jar",
                "E:/desktop/test-jar/druid-1.2.5.jar",
                "E:/desktop/test-jar/httpclient-4.5.13.jar",
                "E:/desktop/test-jar/jackson-core-2.12.1.jar"
        ), jarInfoMap, new HashSet<>()));
    }
}
