package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.jar.OuterInnerJarPath;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/2/8
 * @description:
 */
public class JavaCG2JarUtil {

    private static final Logger logger = LoggerFactory.getLogger(JavaCG2JarUtil.class);

    /**
     * 从jar包中的文件路径获取文件名称
     *
     * @param jarEntryPath jar包中的文件路径
     * @return
     */
    public static String getJarEntryNameFromPath(String jarEntryPath) {
        return JavaCG2Util.getSubStringAfterLast(jarEntryPath, JavaCG2Constants.FLAG_SLASH);
    }

    /**
     * 获得生成的jar包中的目录名称，使用jar包序号与名称合并的结果，以支持同名的文件/目录
     *
     * @param jarNum     jar包序号
     * @param jarDirName jar文件或目录的名称
     * @return
     */
    public static String genDirNameInJar(int jarNum, String jarDirName) {
        String dirName = String.format("%04d%s%s", jarNum, JavaCG2Constants.JAR_SEQ_FLAG, jarDirName);
        logger.info("获得生成的jar包中的目录名称 {}", dirName);
        return dirName;
    }

    /**
     * 根据生成的jar包中的目录名称，获得对应的jar包序号
     *
     * @param dirNameInJar
     * @return
     */
    public static Integer getJarNumFromDirName(String dirNameInJar) {
        String jarNumStr = StringUtils.substringBefore(dirNameInJar, JavaCG2Constants.JAR_SEQ_FLAG);
        return Integer.valueOf(jarNumStr);
    }

    /**
     * 将外层jar/war文件路径，与内层jar文件路径拼接为一个字符串
     *
     * @param outerJarPath
     * @param innerJarPath
     * @return
     */
    public static String mergeOuterInnerJarPath(String outerJarPath, String innerJarPath) {
        return outerJarPath + JavaCG2Constants.NEW_LINE + innerJarPath;
    }

    /**
     * 解析外层jar/war文件路径，与内层jar文件路径拼接后的字符串
     *
     * @param mergedJarPath
     * @return
     */
    public static OuterInnerJarPath parseOuterInnerJarPath(String mergedJarPath) {
        OuterInnerJarPath outerInnerJarPath = new OuterInnerJarPath();
        if (!mergedJarPath.contains(JavaCG2Constants.NEW_LINE)) {
            outerInnerJarPath.setOuterJarPath(mergedJarPath);
            return outerInnerJarPath;
        }

        String[] array = mergedJarPath.split(JavaCG2Constants.NEW_LINE);
        outerInnerJarPath.setOuterJarPath(array[0]);
        outerInnerJarPath.setInnerJarPath(array[1]);
        return outerInnerJarPath;
    }

    /**
     * 获取jar文件中所有的文件及目录的路径
     *
     * @param jarFilePath
     * @return
     */
    public static List<String> getAllPathListInJar(String jarFilePath) {
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(jarFilePath)))) {
            List<String> pathList = new ArrayList<>();
            LocalFileHeader fileHeader;
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                pathList.add(fileHeader.getFileName());
            }
            return pathList;
        } catch (Exception e) {
            logger.error("获取jar文件中所有的文件及目录的路径异常 ", e);
            return Collections.emptyList();
        }
    }

    private JavaCG2JarUtil() {
        throw new IllegalStateException("illegal");
    }
}
