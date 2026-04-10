package test.entry;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;

/**
 * @author adrninistrator
 * @date 2026/2/16
 * @description:
 */
public class TestJavaCG2EntryJVM {

    public static void main(String[] args) {
        System.setProperty(JavaCG2Constants.JVM_PROP_KEY_INPUT_ROOT_PATH, ".");
        new JavaCG2Entry().run();
    }
}
