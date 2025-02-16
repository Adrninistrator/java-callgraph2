package com.adrninistrator.javacg2.parser;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2JarUtil;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，基类
 */
public abstract class AbstractJarEntryParser {

    private static final Logger logger = LoggerFactory.getLogger(AbstractJarEntryParser.class);

    protected String simpleClassName = this.getClass().getSimpleName();

    protected JavaCG2InputAndOutput javaCG2InputAndOutput;

    // 是否只指定了一个需要处理的jar/war文件
    protected boolean onlyOneJar;

    // 处理class文件时，缓存当前处理的文件的第一层目录名及对应jar包信息
    protected String lastFirstDirName;

    // 最近一次处理的jar包序号
    protected Integer lastJarNum;

    protected AbstractJarEntryParser(JavaCG2InputAndOutput javaCG2InputAndOutput, boolean onlyOneJar) {
        this.javaCG2InputAndOutput = javaCG2InputAndOutput;
        this.onlyOneJar = onlyOneJar;
    }

    /**
     * 解析指定的jar包
     *
     * @param jarFilePath
     * @return
     */
    public boolean parse(String jarFilePath) {
        // 初始化
        init();

        String jarEntryPath = null;
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(jarFilePath)))) {
            LocalFileHeader fileHeader;
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                if (fileHeader.isDirectory()) {
                    continue;
                }

                jarEntryPath = fileHeader.getFileName();
                logger.debug("{} 处理文件: {}", simpleClassName, jarEntryPath);

                // 获取当前处理的jar包信息
                if (!handleCurrentJarInfo(jarEntryPath)) {
                    return false;
                }

                // 处理jar包中任意类型的文件
                if (!handleEntry(zipInputStream, jarEntryPath)) {
                    return false;
                }
            }

            return true;
        } catch (Exception e) {
            logger.error("{} 处理jar包中的文件出现问题 {}", simpleClassName, jarEntryPath, e);
            return false;
        }
    }

    // 初始化
    public void init() {
    }

    /**
     * 处理jar包中任意类型的文件
     *
     * @param zipInputStream
     * @param jarEntryPath
     * @return true: 处理成功 false: 处理失败
     */
    protected abstract boolean handleEntry(ZipInputStream zipInputStream, String jarEntryPath) throws IOException;

    /**
     * 尝试处理jar包中的class文件
     *
     * @param inputStream
     * @param jarEntryPath
     * @return true: 是class文件 false: 不是class文件
     */
    protected boolean tryHandleClassEntry(InputStream inputStream, String jarEntryPath) throws IOException {
        if (!JavaCG2FileUtil.isClassFile(jarEntryPath)) {
            return false;
        }

        JavaClass javaClass = new ClassParser(inputStream, jarEntryPath).parse();
        // 处理jar包中的class文件
        handleClassEntry(javaClass, jarEntryPath);
        return true;
    }

    /**
     * 处理jar包中的class文件
     *
     * @param javaClass
     * @param jarEntryPath
     * @return true: 处理成功 false: 处理失败
     */
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryPath) throws IOException {
        return true;
    }

    /**
     * 获取当前处理的jar包信息
     *
     * @param jarEntryPath
     * @return true: 处理成功 false: 处理失败
     */
    private boolean handleCurrentJarInfo(String jarEntryPath) {
        if (onlyOneJar) {
            // 只有一个jar包
            if (lastJarNum == null) {
                lastJarNum = JavaCG2Constants.JAR_NUM_MIN;
            }
            return true;
        }

        // jar包数量大于1个，从Map取值时使用当前JarEntry的第一层目录名称
        int index = jarEntryPath.indexOf(JavaCG2Constants.FLAG_SLASH);
        if (index == -1) {
            logger.error("JarEntry名称中不包含/ {}", jarEntryPath);
            return false;
        }

        String firstDirName = jarEntryPath.substring(0, index);
        if (lastFirstDirName != null && lastFirstDirName.equals(firstDirName)) {
            // 第一层目录名未变化时，使用缓存数据
            return true;
        }
        lastFirstDirName = firstDirName;

        // 首次处理，或第一层目录名变化时，需要从第一层目录名获取jar包序号
        lastJarNum = JavaCG2JarUtil.getJarNumFromDirName(firstDirName);
        return true;
    }
}
