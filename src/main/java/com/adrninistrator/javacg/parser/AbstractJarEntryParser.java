package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，基类
 */
public abstract class AbstractJarEntryParser {

    protected String simpleClassName = this.getClass().getSimpleName();

    protected JavaCGConfInfo javaCGConfInfo;

    protected AbstractJarEntryParser(JavaCGConfInfo javaCGConfInfo) {
        this.javaCGConfInfo = javaCGConfInfo;
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

        String jarEntryName = null;
        try (JarInputStream jarInputStream = new JarInputStream(new BufferedInputStream(new FileInputStream(jarFilePath), 1024 * 32))) {
            JarEntry jarEntry;
            while ((jarEntry = jarInputStream.getNextJarEntry()) != null) {
                if (jarEntry.isDirectory()) {
                    continue;
                }

                jarEntryName = jarEntry.getName();

                if (JavaCGLogUtil.isDebugPrintFlag()) {
                    JavaCGLogUtil.debugPrint(simpleClassName + " 处理文件: " + jarEntryName);
                }

                // 处理jar包中任意类型的文件
                if (!handleEntry(jarInputStream, jarEntryName)) {
                    return false;
                }
            }

            return true;
        } catch (Exception e) {
            System.err.println(simpleClassName + " 处理jar包中的文件出现问题: " + jarEntryName);
            e.printStackTrace();
            return false;
        }
    }

    // 初始化
    protected void init() {
    }

    /**
     * 处理jar包中任意类型的文件
     *
     * @param jarInputStream
     * @param jarEntryName
     * @return true: 处理成功 false: 处理失败
     */
    protected abstract boolean handleEntry(JarInputStream jarInputStream, String jarEntryName) throws IOException;

    /**
     * 尝试处理jar包中的class文件
     *
     * @param inputStream
     * @param jarEntryName
     * @return true: 是class文件 false: 不是class文件
     */
    protected boolean tryHandleClassEntry(InputStream inputStream, String jarEntryName) throws IOException {
        if (!JavaCGFileUtil.isClassFile(jarEntryName)) {
            return false;
        }

        JavaClass javaClass = new ClassParser(inputStream, jarEntryName).parse();
        // 处理jar包中的class文件
        handleClassEntry(javaClass, jarEntryName);
        return true;
    }

    /**
     * 处理jar包中的class文件
     *
     * @param javaClass
     * @param jarEntryName
     * @return true: 处理成功 false: 处理失败
     */
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryName) throws IOException {
        return true;
    }
}
