package merge_jar;

import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.util.HandleJarUtil;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class TestHandleJarUtilDirFirst {

    public static void main(String[] args) {
        Map<String, JarInfo> jarInfoMap = new HashMap<>();

        System.out.println(HandleJarUtil.handleJar(new String[]{
                "E:/desktop/test-dir/out",
                "E:/desktop/test-jar/httpclient-4.5.13.jar",
                "E:/desktop/test-jar/jackson-core-2.12.1.jar"
        }, jarInfoMap));
    }
}
