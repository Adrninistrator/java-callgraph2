package test.parse;

import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;
import org.junit.Assert;
import org.junit.Test;
import test.base.TestBase;

/**
 * @author adrninistrator
 * @date 2025/7/6
 * @description:
 */
public class TestMergeFatJar extends TestBase {

    /*
        合并fat jar，普通jar文件
        需要先在java-all-call-graph项目执行 gradlew test_gen_jar 命令生成对应jar文件
     */
    @Test
    public void testMergeFatJarNormal() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper4JACGTest();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_MERGE_SEPARATE_FAT_JAR, Boolean.TRUE.toString());
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    /*
        合并fat jar，包含jar文件的jar文件
        需要先在java-all-call-graph项目执行 gradlew gen_run_jar gen_jar_in_jar 命令生成对应war文件
     */
    @Test
    public void testMergeFatJarInJar() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper(true, "build/jar_output_dir.jar");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_MERGE_SEPARATE_FAT_JAR, Boolean.TRUE.toString());
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    /*
        合并fat jar，war文件
        需要先在java-all-call-graph项目执行 gradlew gen_run_jar gen_jar_in_war 命令生成对应war文件
     */
    @Test
    public void testMergeFatJarWar() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper(true, "build/jar_output_dir.war");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_MERGE_SEPARATE_FAT_JAR, Boolean.TRUE.toString());
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }

    /*
        合并fat jar，Spring Boot jar文件
        需要先在java-all-call-graph项目执行 gradlew bootJar -b spring_boot.gradle 命令生成对应jar文件
     */
    @Test
    public void testMergeFatJarSPBJar() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = genJavaCG2ConfigureWrapper(true, "build/spring-boot/java-all-call-graph.jar");
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_MERGE_SEPARATE_FAT_JAR, Boolean.TRUE.toString());
        Assert.assertTrue(new JavaCG2Entry(javaCG2ConfigureWrapper).run());
    }
}
