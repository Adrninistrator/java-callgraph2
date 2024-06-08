package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.jar.JarInfo;
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
public class JavaCGJarUtil {

    private static final Logger logger = LoggerFactory.getLogger(JavaCGJarUtil.class);

    // 获取合并jar/war包中的class文件时，需要合并的特定包名
    private static Set<String> getMergeClassInJarPackageSet(Set<String> needHandlePackageSet) {
        if (JavaCGUtil.isCollectionEmpty(needHandlePackageSet)) {
            return new HashSet<>();
        }

        Set<String> mergeClassInJarPackageSet = new HashSet<>(needHandlePackageSet.size());

        for (String mergeClassInJarPackage : needHandlePackageSet) {
            if (StringUtils.isBlank(mergeClassInJarPackage)) {
                continue;
            }

            String newMergeClassInJarPackage = mergeClassInJarPackage.replace(".", "/");

            /*
                使包名替换为路径后，满足以下要求
                不以/开头
                以/结尾
             */
            if (newMergeClassInJarPackage.startsWith("/")) {
                newMergeClassInJarPackage = newMergeClassInJarPackage.substring(1);
                if (StringUtils.isBlank(newMergeClassInJarPackage)) {
                    continue;
                }
            }
            if (!newMergeClassInJarPackage.endsWith("/")) {
                newMergeClassInJarPackage += "/";
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
     * @param jarInfoMap             保存需要处理的jar包文件名及对应的序号，序号从1开始
     * @param needHandlePackageSet
     * @param jarDirMergeFileTypeSet
     * @return null: 处理失败，非null: 新生成的jar包文件，或原有的jar包文件
     */
    public static File handleJar(List<String> jarOrDirPathList, Map<String, JarInfo> jarInfoMap, Set<String> needHandlePackageSet, Set<String> jarDirMergeFileTypeSet) {
        JavaCGCounter jarNumCounter = new JavaCGCounter(JavaCGConstants.RECORD_ID_MIN);
        if (jarOrDirPathList.size() == 1) {
            // 数组只指定了一个元素
            File oneFile = new File(jarOrDirPathList.get(0));
            String oneFilePath = JavaCGFileUtil.getCanonicalPath(oneFile);

            if (!oneFile.exists()) {
                logger.error("指定的jar包或目录不存在: {}", oneFilePath);
                return null;
            }

            if (oneFile.isFile()) {
                String oneFileNameLower = oneFile.getName().toLowerCase(Locale.ROOT);
                if (!StringUtils.endsWithAny(oneFileNameLower, JavaCGConstants.EXT_JAR, JavaCGConstants.EXT_WAR)) {
                    logger.error("处理单个文件时只支持指定 {} 或 {} 格式，假如需要处理 {} 格式的文件，则需要指定其所在目录", JavaCGConstants.EXT_JAR, JavaCGConstants.EXT_WAR, JavaCGConstants.EXT_CLASS);
                    return null;
                }

                // 指定的是一个jar包，直接返回
                // 记录jar包信息，向map中保存数据的key使用固定值
                jarInfoMap.put(oneFile.getName(), new JarInfo(jarNumCounter.addAndGet(), JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, oneFilePath));
                return oneFile;
            }
        }

        // 指定的是一个目录，或数组指定了多于一个元素，需要生成新的jar包
        return mergeJar(jarOrDirPathList, jarInfoMap, needHandlePackageSet, jarDirMergeFileTypeSet, jarNumCounter);
    }

    /**
     * 合并jar包
     * 将每个jar包或目录生成一个新的jar包，第一层目录名为原jar包或目录名
     * 若指定的数组第一个元素为jar包，则新生成的jar包生成在同一个目录中
     * 若指定的数组第一个元素为目录，则新生成的jar包生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar包
     *
     * @param jarOrDirPathList
     * @param jarInfoMap
     * @param needHandlePackageSet
     * @param jarDirMergeFileTypeSet
     * @param jarNumCounter
     * @return 合并后的jar包文件路径
     */
    private static File mergeJar(List<String> jarOrDirPathList, Map<String, JarInfo> jarInfoMap, Set<String> needHandlePackageSet, Set<String> jarDirMergeFileTypeSet,
                                 JavaCGCounter jarNumCounter) {
        // 获取文件或目录列表
        List<File> jarFileOrDirList = getJarFileOrDirList(jarOrDirPathList, jarInfoMap, jarNumCounter);
        if (jarFileOrDirList == null) {
            return null;
        }

        // 获得新的jar包文件
        File newJarFile = getNewJarFile(jarFileOrDirList.get(0), jarOrDirPathList.get(0));
        if (newJarFile.exists()) {
            // 新的jar包文件已存在
            if (newJarFile.isDirectory()) {
                logger.error("新的jar包文件已存在，但是是目录: {}", JavaCGFileUtil.getCanonicalPath(newJarFile));
                return null;
            } else if (!JavaCGFileUtil.deleteFile(newJarFile)) {
                logger.error("新的jar包文件已存在，删除失败: {}", JavaCGFileUtil.getCanonicalPath(newJarFile));
                return null;
            }
        }

        // 已添加到目标jar包中的目录名称
        Set<String> destJarDirNameSet = new HashSet<>(jarOrDirPathList.size());

        // 目录中的jar包文件对象列表
        List<File> jarFileInDirList = new ArrayList<>();

        // 获取合并jar/war包中的class文件时，需要合并的特定包名
        Set<String> mergeClassInJarPackageSet = getMergeClassInJarPackageSet(needHandlePackageSet);

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(newJarFile))) {
            // 合并参数中指定的jar包，以及目录中的后缀非.jar/.war文件
            for (File jarFileOrDir : jarFileOrDirList) {
                String jarFileOrDirName = jarFileOrDir.getName();
                if (destJarDirNameSet.contains(jarFileOrDirName)) {
                    logger.error("指定的jar/war包或目录存在同名，不处理: {} {}", jarFileOrDirName, JavaCGFileUtil.getCanonicalPath(jarFileOrDir));
                    continue;
                }
                destJarDirNameSet.add(jarFileOrDirName);

                if (jarFileOrDir.isFile()) {
                    // 将jar/war包添加到jar包中
                    addJar2Jar(jarFileOrDir, zos, mergeClassInJarPackageSet, jarDirMergeFileTypeSet);
                    continue;
                }

                // 将目录添加到jar包中
                addDir2Jar(jarFileOrDir, jarFileInDirList, zos, jarDirMergeFileTypeSet);
            }

            // 合并目录中的后缀为.jar/.war文件
            for (File jarFileInDir : jarFileInDirList) {
                String jarFileName = jarFileInDir.getName();
                String jarCanonicalPath = JavaCGFileUtil.getCanonicalPath(jarFileInDir);
                // 记录jar包信息，不覆盖现有值
                jarInfoMap.putIfAbsent(jarFileName, new JarInfo(jarNumCounter.addAndGet(), JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, jarCanonicalPath));

                if (destJarDirNameSet.contains(jarFileName)) {
                    logger.error("指定的jar包或目录存在同名，不处理: {} {}", jarFileName, jarCanonicalPath);
                    continue;
                }
                destJarDirNameSet.add(jarFileName);

                logger.info("添加目录中的jar/war包: {}", jarCanonicalPath);
                // 将jar/war包添加到jar包中
                addJar2Jar(jarFileInDir, zos, mergeClassInJarPackageSet, jarDirMergeFileTypeSet);
            }

            return newJarFile;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    /**
     * 获取文件或目录列表
     *
     * @param jarOrDirPathList
     * @param jarInfoMap
     * @param jarNumCounter
     * @return
     */
    private static List<File> getJarFileOrDirList(List<String> jarOrDirPathList, Map<String, JarInfo> jarInfoMap, JavaCGCounter jarNumCounter) {
        List<File> jarFileOrDirList = new ArrayList<>(jarOrDirPathList.size());

        for (String currentJarOrDirPath : jarOrDirPathList) {
            File jarFileOrDir = new File(currentJarOrDirPath);
            String jarCanonicalPath = JavaCGFileUtil.getCanonicalPath(jarFileOrDir);
            if (!jarFileOrDir.exists() || jarCanonicalPath == null) {
                logger.error("指定的jar包或目录不存在: {}", jarCanonicalPath);
                return null;
            }
            if (jarCanonicalPath.endsWith(JavaCGConstants.MERGED_JAR_FLAG)) {
                // 假如指定了合并产生的jar包，且指定的jar包数量大于1，则跳过
                logger.info("跳过合并产生的jar包: {}", jarCanonicalPath);
                continue;
            }

            jarFileOrDirList.add(jarFileOrDir);

            String jarOrDirType = jarFileOrDir.isFile() ? JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX : JavaCGConstants.FILE_KEY_DIR_INFO_PREFIX;
            // 记录jar包信息，不覆盖现有值
            jarInfoMap.putIfAbsent(jarFileOrDir.getName(), new JarInfo(jarNumCounter.addAndGet(), jarOrDirType, jarCanonicalPath));
        }

        return jarFileOrDirList;
    }

    // 获得新的jar包文件
    private static File getNewJarFile(File firstJarFile, String firstJarPath) {
        if (firstJarFile.isFile()) {
            // 数组第一个元素为jar包
            return new File(firstJarPath + JavaCGConstants.MERGED_JAR_FLAG);
        }

        // 数组第一个元素为目录
        return new File(firstJarPath + File.separator + firstJarFile.getName() + JavaCGConstants.MERGED_JAR_FLAG);
    }

    // 将jar/war包添加到jar包中
    private static void addJar2Jar(File sourceJarFile, ZipOutputStream targetZos, Set<String> mergeClassInJarPackageSet, Set<String> jarDirMergeFileTypeSet) throws IOException {
        String sourceJarName = sourceJarFile.getName();

        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile), 1024 * 8))) {
            LocalFileHeader fileHeader;
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                if (fileHeader.isDirectory()) {
                    // 跳过目录
                    continue;
                }

                String jarEntryPath = fileHeader.getFileName();
                // 判断当前文件是否需要跳过
                if (StringUtils.endsWithIgnoreCase(jarEntryPath, JavaCGConstants.EXT_CLASS)) {
                    // class文件，判断是否需要跳过
                    if (skipClassEntry(jarEntryPath, mergeClassInJarPackageSet)) {
                        continue;
                    }
                } else if (!JavaCGUtil.checkMergeFileType(jarEntryPath, jarDirMergeFileTypeSet)) {
                    // 其他类型的文件，不需要处理
                    logger.debug("当前文件类型不匹配，不合并 {}", jarEntryPath);
                    continue;
                }

                // 处理jar包中的一个非jar文件
                ZipParameters zipParameters = new ZipParameters();
                zipParameters.setFileNameInZip(sourceJarName + "/" + jarEntryPath);
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
                    jarEntryPath.startsWith(JavaCGConstants.WEB_INF_CLASSES + "/" + mergeClassInJarPackage) ||
                    jarEntryPath.startsWith(JavaCGConstants.BOOT_INF_CLASSES + "/" + mergeClassInJarPackage)
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
    private static void addDir2Jar(File sourceDirFile, List<File> jarFileInDirList, ZipOutputStream targetZos, Set<String> jarDirMergeFileTypeSet) throws IOException {
        // 保存后缀非.jar文件对象列表
        List<File> nonJarFileList = new ArrayList<>();
        // 保存后缀非.jar文件的相对路径列表
        List<String> nonJarFileRelativelyPathList = new ArrayList<>();

        // 查找指定目录中不同后缀的文件
        findFileInSubDir(sourceDirFile, null, nonJarFileList, nonJarFileRelativelyPathList, jarFileInDirList);
        if (nonJarFileList.isEmpty()) {
            return;
        }

        for (int i = 0; i < nonJarFileList.size(); i++) {
            String filePath = nonJarFileRelativelyPathList.get(i);
            /*
                判断文件是否需要处理：
                class文件需要处理，不判断文件类名
                其他文件，判断是否为需要处理的类型
             */
            if (!StringUtils.endsWithIgnoreCase(filePath, JavaCGConstants.EXT_CLASS) &&
                    !JavaCGUtil.checkMergeFileType(filePath, jarDirMergeFileTypeSet)) {
                logger.debug("当前文件类型不匹配，不合并 {}", filePath);
                continue;
            }

            ZipParameters zipParameters = new ZipParameters();
            zipParameters.setFileNameInZip(filePath);
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

        String dirPathHeader = (dirPath == null ? dirFile.getName() : dirPath + "/" + dirFile.getName());

        for (File file : files) {
            if (file.isDirectory()) {
                // 递归处理目录
                findFileInSubDir(file, dirPathHeader, nonJarFileList, fileRelativelyPathList, jarFileInDirList);
                continue;
            }

            // 处理文件
            String currentFileName = file.getName();
            if (StringUtils.endsWithAny(currentFileName.toLowerCase(), JavaCGConstants.EXT_JAR, JavaCGConstants.EXT_WAR)) {
                // 目录中的当前文件后缀是.jar/.war
                if (!currentFileName.endsWith(JavaCGConstants.MERGED_JAR_FLAG)) {
                    // 不是合并产生的文件
                    // 记录后缀为.jar/.war文件对象
                    jarFileInDirList.add(file);
                }
            } else if (!currentFileName.contains(JavaCGConstants.MERGED_JAR_FLAG)) {
                // 目录中的当前文件后缀不是.jar/.war，且不包含合并产生的文件标记（对合并文件执行生成的.txt结果文件不需要再合并），需要合并到最终的jar包中
                // 记录后缀非.jar文件的文件对象及相对路径
                nonJarFileList.add(file);
                fileRelativelyPathList.add(dirPathHeader + "/" + currentFileName);
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
        return JavaCGUtil.getSubStringAfterLast(jarEntryPath, "/");
    }

    private JavaCGJarUtil() {
        throw new IllegalStateException("illegal");
    }
}
