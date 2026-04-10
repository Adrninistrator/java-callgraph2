package test.entry;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;

/**
 * @author adrninistrator
 * @date 2026/2/16
 * @description:
 */
public class TestJavaCG2EntryConfigPath {

    public static void main(String[] args) {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper(false, "src/main/resources");
        new JavaCG2Entry(javaCG2ConfigureWrapper).run();
    }
}
