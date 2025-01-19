package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.io.outputstream.ZipOutputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import net.lingala.zip4j.model.ZipParameters;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/2/8
 * @description:
 */
public class JavaCG2JarUtil {

    private static final Logger logger = LoggerFactory.getLogger(JavaCG2JarUtil.class);

    // 获取合并jar/war包中的class文件时，需要合并的特定包名
    private static Set<String> getMergeClassInJarPackageSet(Set<String> needHandlePackageSet) {
        if (JavaCG2Util.isCollectionEmpty(needHandlePackageSet)) {
            return new HashSet<>();
        }

        Set<String> mergeClassInJarPackageSet = new HashSet<>(needHandlePackageSet.size());

        for (String mergeClassInJarPackage : needHandlePackageSet) {
            if (StringUtils.isBlank(mergeClassInJarPackage)) {
                continue;
            }

            String newMergeClassInJarPackage = mergeClassInJarPackage.replace(JavaCG2Constants.FLAG_DOT, JavaCG2Constants.FLAG_SLASH);

            /*
                使包名替换为路径后，满足以下要求
                不以/开头
                以/结尾
             */
            if (newMergeClassInJarPackage.startsWith(JavaCG2Constants.FLAG_SLASH)) {
                newMergeClassInJarPackage = newMergeClassInJarPackage.substring(1);
                if (StringUtils.isBlank(newMergeClassInJarPackage)) {
                    continue;
                }
            }
            if (!newMergeClassInJarPackage.endsWith(JavaCG2Constants.FLAG_SLASH)) {
                newMergeClassInJarPackage += JavaCG2Constants.FLAG_SLASH;
            }

            logger.info("合并jar/war包中的class文件时，需要合并的包名: {}", newMergeClassInJarPackage);

            mergeClassInJarPackageSet.add(newMergeClassInJarPackage);
        }

        return mergeClassInJarPackageSet;
    }

    /**
     * 对指定的jar包进行处理
     * 若指定的数组只有一个元素，且为jar包，则直接返回
     * 其他情况下，需要生成新的jar包
     *
     * @param jarOrDirPathList
     * @param jarPathNumMap
     * @param needHandlePackageSet
     * @param jarDirMergeFileTypeSet
     * @param jarNumCounter
     * @return null: 处理失败，非null: 新生成的jar包文件，或原有的jar包文件
     */
    public static File handleJar(List<String> jarOrDirPathList, Map<String, Integer> jarPathNumMap, Set<String> needHandlePackageSet, Set<String> jarDirMergeFileTypeSet,
                                 JavaCG2Counter jarNumCounter) {
        // 生成的jar_info中的第一个元素是结果目录，以下jar包序号从2开始
        if (jarOrDirPathList.size() == 1) {
            // 数组只指定了一个元素
            File oneFile = new File(jarOrDirPathList.get(0));
            String oneFilePath = JavaCG2FileUtil.getCanonicalPath(oneFile);

            if (!oneFile.exists()) {
                logger.error("指定了一个jar包或目录，不存在: {}", oneFilePath);
                return null;
            }

            if (oneFile.isFile()) {
                String oneFileNameLower = oneFile.getName().toLowerCase(Locale.ROOT);
                if (!StringUtils.endsWithAny(oneFileNameLower, JavaCG2Constants.EXT_JAR, JavaCG2Constants.EXT_WAR)) {
                    logger.error("处理单个文件时只支持指定 {} 或 {} 格式，假如需要处理 {} 格式的文件，则需要指定其所在目录", JavaCG2Constants.EXT_JAR, JavaCG2Constants.EXT_WAR, JavaCG2Constants.EXT_CLASS);
                    return null;
                }

                // 指定的是一个jar包，直接返回
                // 记录jar包信息，向map中保存数据的key使用固定值
                int jarNum = jarNumCounter.addAndGet();
                jarPathNumMap.put(oneFilePath, jarNum);
                return oneFile;
            }
        }

        // 指定的是一个目录，或数组指定了多于一个元素，需要生成新的jar包
        return mergeJar(jarOrDirPathList, jarPathNumMap, needHandlePackageSet, jarDirMergeFileTypeSet, jarNumCounter);
    }

    /**
     * 合并jar包
     * 将每个jar包或目录生成一个新的jar包，第一层目录名为原jar包或目录名
     * 若指定的数组第一个元素为jar包，则新生成的jar包生成在同一个目录中
     * 若指定的数组第一个元素为目录，则新生成的jar包生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar包
     *
     * @param jarOrDirPathList
     * @param jarPathNumMap
     * @param needHandlePackageSet
     * @param jarDirMergeFileTypeSet
     * @param jarNumCounter
     * @return 合并后的jar包文件路径
     */
    private static File mergeJar(List<String> jarOrDirPathList, Map<String, Integer> jarPathNumMap, Set<String> needHandlePackageSet, Set<String> jarDirMergeFileTypeSet,
                                 JavaCG2Counter jarNumCounter) {
        // 获取文件或目录列表
        List<File> jarFileOrDirList = getJarFileOrDirList(jarOrDirPathList, jarPathNumMap, jarNumCounter);
        if (jarFileOrDirList == null) {
            return null;
        }

        // 获得新的jar包文件
        File newJarFile = getNewJarFile(jarFileOrDirList.get(0), jarOrDirPathList.get(0));
        if (newJarFile.exists()) {
            // 新的jar包文件已存在
            if (newJarFile.isDirectory()) {
                logger.error("新的jar包文件已存在，但是是目录: {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
                return null;
            } else if (!JavaCG2FileUtil.deleteFile(newJarFile)) {
                logger.error("新的jar包文件已存在，删除失败: {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
                return null;
            }
        }

        // 目录中的jar包文件对象列表
        List<File> jarFileInDirList = new ArrayList<>();

        // 获取合并jar/war包中的class文件时，需要合并的特定包名
        Set<String> mergeClassInJarPackageSet = getMergeClassInJarPackageSet(needHandlePackageSet);

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(newJarFile))) {
            // 合并参数中指定的jar包，以及目录中的后缀非.jar/.war文件
            for (File jarFileOrDir : jarFileOrDirList) {
                String jarOrDirCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileOrDir);
                int jarNum = jarPathNumMap.get(jarOrDirCanonicalPath);
                String dirNameInJar = genDirNameInJar(jarNum, jarFileOrDir.getName());
                if (jarFileOrDir.isFile()) {
                    // 将jar/war包添加到jar包中
                    addJar2Jar(jarFileOrDir, dirNameInJar, zos, mergeClassInJarPackageSet, jarDirMergeFileTypeSet);
                    continue;
                }

                // 将目录添加到jar包中
                addDir2Jar(jarFileOrDir, dirNameInJar, jarFileInDirList, zos, jarDirMergeFileTypeSet);
            }

            // 合并目录中的后缀为.jar/.war文件
            for (File jarFileInDir : jarFileInDirList) {
                String jarCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileInDir);
                if (jarPathNumMap.containsKey(jarCanonicalPath)) {
                    // 避免在jar_dir配置文件中指定了目录与jar包时，目录中包含对应jar包时重复处理对应jar包
                    logger.warn("目录中的jar/war包在处理对应目录时已处理过，不再处理 {}", jarCanonicalPath);
                    continue;
                }

                int jarNum = jarNumCounter.addAndGet();
                jarPathNumMap.put(jarCanonicalPath, jarNum);
                String dirNameInJar = genDirNameInJar(jarNum, jarFileInDir.getName());

                logger.info("添加目录中的jar/war包: {}", jarCanonicalPath);
                // 将jar/war包添加到jar包中
                addJar2Jar(jarFileInDir, dirNameInJar, zos, mergeClassInJarPackageSet, jarDirMergeFileTypeSet);
            }

            return newJarFile;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    // 获取文件或目录列表
    private static List<File> getJarFileOrDirList(List<String> jarOrDirPathList, Map<String, Integer> jarPathNumMap, JavaCG2Counter jarNumCounter) {
        List<File> jarFileOrDirList = new ArrayList<>(jarOrDirPathList.size());
        Set<String> jarFileOrDirPathSet = new HashSet<>();

        for (String currentJarOrDirPath : jarOrDirPathList) {
            File jarFileOrDir = new File(currentJarOrDirPath);
            String jarCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileOrDir);
            if (!jarFileOrDir.exists() || jarCanonicalPath == null) {
                logger.error("指定的jar包或目录不存在: {}", jarCanonicalPath);
                return null;
            }
            if (jarCanonicalPath.endsWith(JavaCG2Constants.MERGED_JAR_FLAG)) {
                // 假如指定了合并产生的jar包，且指定的jar包数量大于1，则跳过
                logger.info("跳过合并产生的jar包: {}", jarCanonicalPath);
                continue;
            }
            if (!jarFileOrDirPathSet.add(jarCanonicalPath)) {
                logger.warn("跳过重复的jar包/目录 {} {}", currentJarOrDirPath, jarCanonicalPath);
                continue;
            }
            jarFileOrDirList.add(jarFileOrDir);
            jarPathNumMap.put(jarCanonicalPath, jarNumCounter.addAndGet());
        }

        return jarFileOrDirList;
    }

    // 获得新的jar包文件
    private static File getNewJarFile(File firstJarFile, String firstJarPath) {
        if (firstJarFile.isFile()) {
            // 数组第一个元素为jar包
            return new File(firstJarPath + JavaCG2Constants.MERGED_JAR_FLAG);
        }

        // 数组第一个元素为目录
        return new File(firstJarPath + File.separator + firstJarFile.getName() + JavaCG2Constants.MERGED_JAR_FLAG);
    }

    // 将jar/war包添加到jar包中
    private static void addJar2Jar(File sourceJarFile, String firstLevelDirName, ZipOutputStream targetZos, Set<String> mergeClassInJarPackageSet,
                                   Set<String> jarDirMergeFileTypeSet) throws IOException {
        String sourceJarFilePath = sourceJarFile.getAbsolutePath();
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile), 1024 * 8))) {
            LocalFileHeader fileHeader;
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                if (fileHeader.isDirectory()) {
                    // 跳过目录
                    continue;
                }

                String jarEntryPath = fileHeader.getFileName();
                // 判断当前文件是否需要跳过
                if (StringUtils.endsWithIgnoreCase(jarEntryPath, JavaCG2Constants.EXT_CLASS)) {
                    // class文件，判断是否需要跳过
                    if (skipClassEntry(jarEntryPath, mergeClassInJarPackageSet)) {
                        continue;
                    }
                } else if (!JavaCG2Util.checkMergeFileType(jarEntryPath, jarDirMergeFileTypeSet)) {
                    // 其他类型的文件，不需要处理
                    logger.debug("Jar中的当前文件类型不匹配，不合并 {} {}", sourceJarFilePath, jarEntryPath);
                    continue;
                }

                // 处理jar包中的一个非jar文件
                ZipParameters zipParameters = new ZipParameters();
                zipParameters.setFileNameInZip(firstLevelDirName + JavaCG2Constants.FLAG_SLASH + jarEntryPath);
                targetZos.putNextEntry(zipParameters);

                // 向目标jar文件写入数据
                addInput2Jar(zipInputStream, targetZos);
            }
        }
    }

    /**
     * 是否跳过class文件
     *
     * @param jarEntryPath
     * @param mergeClassInJarPackageSet
     * @return true: 跳过 false: 不跳过
     */
    private static boolean skipClassEntry(String jarEntryPath, Set<String> mergeClassInJarPackageSet) {
        if (mergeClassInJarPackageSet.isEmpty()) {
            // 不跳过class文件
            return false;
        }

        // 根据class文件包名进行处理
        for (String mergeClassInJarPackage : mergeClassInJarPackageSet) {
            /*
                war包中的class文件在WEB-INF/classes/目录中
                Spring Boot Maven Plugin插件打包后的jar包，class文件在BOOT-INF/classes/目录中
             */
            if (jarEntryPath.startsWith(mergeClassInJarPackage) ||
                    jarEntryPath.startsWith(JavaCG2Constants.WEB_INF_CLASSES + JavaCG2Constants.FLAG_SLASH + mergeClassInJarPackage) ||
                    jarEntryPath.startsWith(JavaCG2Constants.BOOT_INF_CLASSES + JavaCG2Constants.FLAG_SLASH + mergeClassInJarPackage)
            ) {
                logger.debug("当前class文件包名匹配，需要合并 {} {}", jarEntryPath, mergeClassInJarPackage);
                // 不跳过包名满足要求的class文件
                return false;
            }
        }

        // 跳过包名不满足要求的class文件
        logger.debug("当前class文件包名或类名不匹配，不合并 {}", jarEntryPath);
        return true;
    }

    // 将目录添加到jar包中
    private static void addDir2Jar(File sourceDirFile, String firstLevelDirName, List<File> jarFileInDirList, ZipOutputStream targetZos, Set<String> jarDirMergeFileTypeSet) throws IOException {
        // 保存后缀非.jar文件对象列表
        List<File> nonJarFileList = new ArrayList<>();
        // 保存后缀非.jar文件的相对路径列表
        List<String> nonJarFileRelativelyPathList = new ArrayList<>();

        // 查找指定目录中不同后缀的文件
        findFileInSubDir(sourceDirFile, null, nonJarFileList, nonJarFileRelativelyPathList, jarFileInDirList);
        if (nonJarFileList.isEmpty()) {
            return;
        }

        String sourceDirFilePath = sourceDirFile.getAbsolutePath();
        for (int i = 0; i < nonJarFileList.size(); i++) {
            String filePath = nonJarFileRelativelyPathList.get(i);
            /*
                判断文件是否需要处理：
                class文件需要处理，不判断文件类名
                其他文件，判断是否为需要处理的类型
             */
            if (!StringUtils.endsWithIgnoreCase(filePath, JavaCG2Constants.EXT_CLASS) &&
                    !JavaCG2Util.checkMergeFileType(filePath, jarDirMergeFileTypeSet)) {
                logger.debug("目录中的当前文件类型不匹配，不合并 {} {}", sourceDirFilePath, filePath);
                continue;
            }

            // 修改写入jar包中的文件名的第一层目录
            String filePathTail = StringUtils.substringAfter(filePath, JavaCG2Constants.FLAG_SLASH);
            String newFilePath = firstLevelDirName + JavaCG2Constants.FLAG_SLASH + filePathTail;

            ZipParameters zipParameters = new ZipParameters();
            zipParameters.setFileNameInZip(newFilePath);
            targetZos.putNextEntry(zipParameters);

            // 向目标jar文件写入数据
            try (InputStream inputStream = new FileInputStream(nonJarFileList.get(i))) {
                addInput2Jar(inputStream, targetZos);
            }
        }
    }

    // 查找指定目录中不同后缀的文件
    private static void findFileInSubDir(File dirFile, String dirPath, List<File> nonJarFileList, List<String> fileRelativelyPathList, List<File> jarFileInDirList) {
        File[] files = dirFile.listFiles();
        if (files == null) {
            return;
        }

        String dirPathHeader = (dirPath == null ? dirFile.getName() : dirPath + JavaCG2Constants.FLAG_SLASH + dirFile.getName());

        for (File file : files) {
            if (file.isDirectory()) {
                // 递归处理目录，需要跳过保存处理失败class文件的子目录
                if (!JavaCG2Constants.DIR_FAIL_CLASSES.equals(file.getName())) {
                    findFileInSubDir(file, dirPathHeader, nonJarFileList, fileRelativelyPathList, jarFileInDirList);
                }
                continue;
            }

            // 处理文件
            String currentFileName = file.getName();
            if (StringUtils.endsWithAny(currentFileName.toLowerCase(), JavaCG2Constants.EXT_JAR, JavaCG2Constants.EXT_WAR)) {
                // 目录中的当前文件后缀是.jar/.war
                if (!currentFileName.endsWith(JavaCG2Constants.MERGED_JAR_FLAG)) {
                    // 不是合并产生的文件
                    // 记录后缀为.jar/.war文件对象
                    jarFileInDirList.add(file);
                }
            } else if (!currentFileName.contains(JavaCG2Constants.MERGED_JAR_FLAG)) {
                // 目录中的当前文件后缀不是.jar/.war，且不包含合并产生的文件标记（对合并文件执行生成的.txt结果文件不需要再合并），需要合并到最终的jar包中
                // 记录后缀非.jar文件的文件对象及相对路径
                nonJarFileList.add(file);
                fileRelativelyPathList.add(dirPathHeader + JavaCG2Constants.FLAG_SLASH + currentFileName);
            }
        }
    }

    // 向目标jar文件写入数据
    private static void addInput2Jar(InputStream inputStream, ZipOutputStream targetZos) throws IOException {
        byte[] data = new byte[8192];
        int len;
        while ((len = inputStream.read(data)) > 0) {
            targetZos.write(data, 0, len);
        }
        targetZos.closeEntry();
    }

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
     * @param jarNum
     * @param jarDirName
     * @return
     */
    private static String genDirNameInJar(int jarNum, String jarDirName) {
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

    private JavaCG2JarUtil() {
        throw new IllegalStateException("illegal");
    }
}
