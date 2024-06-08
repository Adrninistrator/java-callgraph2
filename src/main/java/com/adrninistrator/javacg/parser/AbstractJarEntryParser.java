package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGJarUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
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
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，基类
 */
public abstract class AbstractJarEntryParser {

    private static final Logger logger = LoggerFactory.getLogger(AbstractJarEntryParser.class);

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

        String jarEntryPath = null;
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(jarFilePath), 1024 * 8))) {
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

                // 判断当前文件是否需要忽略
                if (ignoreCurrentFile(jarEntryPath)) {
                    // 忽略当前的文件
                    continue;
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
        if (!JavaCGFileUtil.isClassFile(jarEntryPath)) {
            return false;
        }

        JavaClass javaClass = new ClassParser(inputStream, jarEntryPath).parse();
        // 判断是否忽略当前类
        if (ignoreCurrentClass(javaClass.getClassName())) {
            return true;
        }

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
        int index = jarEntryPath.indexOf("/");
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

        // 首次处理，或第一层目录名变化时，需要从Map获取
        lastJarInfo = jarInfoMap.get(firstDirName);
        if (lastJarInfo == null) {
            logger.error("合并后的jar包中出现的名称未记录过: {}", jarEntryPath);
        }
        return true;
    }

    /**
     * 判断当前处理的文件是否需要忽略
     *
     * @param jarEntryPath jar包中的文件路径
     * @return true: 需要忽略 false: 不忽略
     */
    private boolean ignoreCurrentFile(String jarEntryPath) {
        for (String ignoreJarFileKeyword : javaCGConfInfo.getIgnoreJarFileKeywordSet()) {
            if (jarEntryPath.contains(ignoreJarFileKeyword)) {
                logger.info("jar包中的当前文件路径包含指定关键字，需要跳过 [{}] [{}]", jarEntryPath, ignoreJarFileKeyword);
                return true;
            }
        }
        String jarEntryName = JavaCGJarUtil.getJarEntryNameFromPath(jarEntryPath);
        for (String ignoreJarFileName : javaCGConfInfo.getIgnoreJarFileNameSet()) {
            if (jarEntryName.equals(ignoreJarFileName)) {
                logger.info("jar包中的当前文件名已设置为需要跳过 [{}] [{}]", jarEntryPath, ignoreJarFileName);
                return true;
            }
        }
        return false;
    }

    /**
     * 根据类名判断当前类是否需要忽略
     *
     * @param className 类名
     * @return true: 忽略当前类 false: 不忽略当前类
     */
    protected boolean ignoreCurrentClass(String className) {
        if (JavaCGUtil.checkSkipClass(className, javaCGConfInfo.getNeedHandlePackageSet())) {
            logger.debug("跳过不需要处理的类: {}", className);
            return true;
        }
        return false;
    }
}
