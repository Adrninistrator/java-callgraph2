package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
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

    /*
        保存jar包或目录的名称对应的jar包信息
        key     jar包或目录的名称
        value   jar包信息
     */
    protected Map<String, JarInfo> jarInfoMap;

    // 处理class文件时，缓存当前处理的文件的第一层目录名及对应jar包信息
    protected String lastFirstDirName;

    // 最近一次处理的jar信息
    protected JarInfo lastJarInfo;

    protected AbstractJarEntryParser(JavaCGConfInfo javaCGConfInfo, Map<String, JarInfo> jarInfoMap) {
        this.javaCGConfInfo = javaCGConfInfo;
        this.jarInfoMap = jarInfoMap;
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

                // 获取当前处理的jar包信息
                if (!handleCurrentJarInfo(jarEntryName)) {
                    return false;
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

    /**
     * 获取当前处理的jar包信息
     *
     * @param jarEntryName
     * @return true: 处理成功 false: 处理失败
     */
    private boolean handleCurrentJarInfo(String jarEntryName) {
        if (jarInfoMap.size() == 1) {
            // 只有一个jar包，从Map取第一个Entry
            if (lastJarInfo == null) {
                // 第一次处理当前jar包
                for (Map.Entry<String, JarInfo> entry : jarInfoMap.entrySet()) {
                    lastJarInfo = entry.getValue();
                    return true;
                }
            }
            return true;
        }

        // jar包数量大于1个，从Map取值时使用当前JarEntry的第一层目录名称
        int index = jarEntryName.indexOf("/");
        if (index == -1) {
            System.err.println("JarEntry名称中不包含/ " + jarEntryName);
            return false;
        }

        String firstDirName = jarEntryName.substring(0, index);
        if (lastFirstDirName != null && lastFirstDirName.equals(firstDirName)) {
            // 第一层目录名未变化时，使用缓存数据
            return true;
        }
        lastFirstDirName = firstDirName;

        // 首次处理，或第一层目录名变化时，需要从Map获取
        lastJarInfo = jarInfoMap.get(firstDirName);
        if (lastJarInfo == null) {
            System.err.println("合并后的jar包中出现的名称未记录过: " + jarEntryName);
        }
        return true;
    }
}
