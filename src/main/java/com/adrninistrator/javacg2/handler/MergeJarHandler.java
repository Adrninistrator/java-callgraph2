package com.adrninistrator.javacg2.handler;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2JarUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.io.outputstream.ZipOutputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import net.lingala.zip4j.model.ZipParameters;
import org.apache.commons.io.FileUtils;
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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/1/25
 * @description: 用于合并jar、war、class文件的处理类
 */
public class MergeJarHandler {

    private static final Logger logger = LoggerFactory.getLogger(MergeJarHandler.class);

    private final List<String> jarOrDirPathList;

    private final Map<String, Integer> jarPathNumMap;

    private final JavaCG2Counter jarNumCounter;

    private final JavaCG2ElManager javaCG2ElManager;

    private long totalSize = 0;

    /**
     * @param javaCG2ConfigureWrapper 需要合并的jar/war文件，或目录的列表
     * @param jarPathNumMap           保存jar文件路径与对应序号的Map
     * @param jarNumCounter           jar文件序号计数器
     * @param javaCG2ElManager        表达式管理类
     */
    public MergeJarHandler(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, Map<String, Integer> jarPathNumMap, JavaCG2Counter jarNumCounter, JavaCG2ElManager javaCG2ElManager) {
        jarOrDirPathList = javaCG2ConfigureWrapper.getOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR);
        if (jarOrDirPathList.isEmpty()) {
            logger.error("请在配置文件 {} 中指定需要处理的jar文件或目录列表", JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey());
            throw new JavaCG2RuntimeException("未指定需要处理的jar文件或目录列表");
        }

        logger.info("需要处理的jar文件或目录:\n{}", StringUtils.join(jarOrDirPathList, "\n"));

        this.jarPathNumMap = jarPathNumMap;
        this.jarNumCounter = jarNumCounter;
        this.javaCG2ElManager = javaCG2ElManager;
    }

    /**
     * 合并指定的jar、war文件或目录
     * 将指定的jar、war文件或目录生成一个新的jar文件，第一层目录名为原jar、war文件或目录名
     * 若指定的jarOrDirPathList第一个元素为jar、war文件，则新生成的jar文件生成在同一个目录中
     * 若指定的jarOrDirPathList第一个元素为目录，则新生成的jar文件生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar文件
     *
     * @return null: 处理失败，非null: 新生成的jar文件文件，或原有的jar文件文件
     */
    public File mergeJar() {
        // 生成的jar_info中的第一个元素是结果目录，以下jar文件序号从2开始
        if (jarOrDirPathList.size() == 1) {
            // List只指定了一个元素
            String jarOrDirPath = jarOrDirPathList.get(0);
            File oneFile = new File(jarOrDirPath);
            String oneFilePath = JavaCG2FileUtil.getCanonicalPath(oneFile);

            if (!oneFile.exists()) {
                logger.error("指定了一个jar文件或目录，不存在: {}", oneFilePath);
                return null;
            }

            if (oneFile.isFile()) {
                if (!JavaCG2FileUtil.checkJarWarFile(oneFile.getName())) {
                    logger.error("处理单个文件时只支持指定 {} 或 {} 格式，假如需要处理 {} 格式的文件，则需要指定其所在目录", JavaCG2Constants.EXT_JAR, JavaCG2Constants.EXT_WAR, JavaCG2Constants.EXT_CLASS);
                    return null;
                }

                // 指定的是一个jar/war文件，判断其中是否有jar文件
                List<String> jarPathList = getJarPathListInJar(jarOrDirPath);
                if (jarPathList.isEmpty()) {
                    // 指定的jar文件中不存在jar文件，记录jar文件信息
                    int jarNum = jarNumCounter.addAndGet();
                    jarPathNumMap.put(oneFilePath, jarNum);
                    logger.info("仅指定了一个jar/war文件，直接使用 {}", jarOrDirPath);
                    return oneFile;
                }
            }
        }

        long startTime = System.currentTimeMillis();
        // 指定的是一个目录，或List指定了多于一个元素，需要生成新的jar文件
        File resultFile = doMergeJar();
        if (resultFile == null) {
            return null;
        }
        String fileSizeDisplay = FileUtils.byteCountToDisplaySize(totalSize);
        logger.info("合并jar文件完毕，耗时 {} 秒，（解压后）总大小约 {}", JavaCG2Util.getSpendSeconds(startTime), fileSizeDisplay);
        return resultFile;
    }

    /**
     * 获取jar/war文件中的jar文件路径列表
     *
     * @param jarFilePath
     * @return
     */
    private List<String> getJarPathListInJar(String jarFilePath) {
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(jarFilePath)))) {
            List<String> jarPathList = new ArrayList<>();
            LocalFileHeader fileHeader;
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                if (fileHeader.isDirectory()) {
                    continue;
                }

                if (StringUtils.endsWithIgnoreCase(fileHeader.getFileName(), JavaCG2Constants.EXT_JAR)) {
                    jarPathList.add(fileHeader.getFileName());
                }
            }
            return jarPathList;
        } catch (Exception e) {
            logger.error("判断jar/war文件中是否存在jar文件异常 {} ", jarFilePath, e);
            return Collections.emptyList();
        }
    }

    /**
     * 执行合并jar/war文件
     *
     * @return 合并后的jar文件文件路径
     */
    private File doMergeJar() {
        // 获取文件或目录列表
        List<File> jarFileOrDirList = getJarFileOrDirList();
        if (jarFileOrDirList == null) {
            return null;
        }

        // 获得新的jar文件文件
        File newJarFile = getNewJarFile(jarFileOrDirList.get(0), jarOrDirPathList.get(0));
        if (newJarFile.exists()) {
            // 新的jar文件文件已存在
            if (newJarFile.isDirectory()) {
                logger.error("新的jar文件文件已存在，但是是目录: {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
                return null;
            } else if (!JavaCG2FileUtil.deleteFile(newJarFile)) {
                logger.error("新的jar文件文件已存在，删除失败: {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
                return null;
            }
        }

        // 目录中的jar文件文件对象列表
        List<File> jarFileInDirList = new ArrayList<>();

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(newJarFile))) {
            // 处理参数中指定的jar文件，以及目录中的后缀非jar/war文件
            for (File jarFileOrDir : jarFileOrDirList) {
                String jarOrDirCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileOrDir);
                int jarNum = jarPathNumMap.get(jarOrDirCanonicalPath);
                // 获得生成的jar文件中的目录名称
                String dirNameInJar = JavaCG2JarUtil.genDirNameInJar(jarNum, jarFileOrDir.getName());
                if (jarFileOrDir.isFile()) {
                    // 记录jar/war文件中的jar文件序号
                    recordJarInJarNum(jarOrDirCanonicalPath);
                    // 处理jar/war文件，将其中的文件添加到目标jar文件中
                    handleJarWarFile(jarFileOrDir, jarOrDirCanonicalPath, dirNameInJar, zos, true);
                    continue;
                }

                // 处理目录，将其中的文件添加到目标jar文件中
                handlerDir(jarFileOrDir, dirNameInJar, jarFileInDirList, zos);
            }

            // 合并从目录中找到的的后缀为jar/war的文件
            for (File jarFileInDir : jarFileInDirList) {
                String jarCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileInDir);
                if (jarPathNumMap.containsKey(jarCanonicalPath)) {
                    // 避免在jar_dir配置文件中指定了目录与jar文件时，目录中包含对应jar文件时重复处理对应jar文件
                    logger.warn("目录中的jar/war文件在处理对应目录时已处理过，不再处理 {}", jarCanonicalPath);
                    continue;
                }

                int jarNum = jarNumCounter.addAndGet();
                jarPathNumMap.put(jarCanonicalPath, jarNum);

                // 记录jar/war文件中的jar文件序号
                recordJarInJarNum(jarCanonicalPath);

                // 获得生成的jar文件中的目录名称
                String dirNameInJar = JavaCG2JarUtil.genDirNameInJar(jarNum, jarFileInDir.getName());

                logger.info("处理目录中的jar/war文件: {}", jarCanonicalPath);
                // 处理jar/war文件，将其中的文件添加到目标jar文件中
                handleJarWarFile(jarFileInDir, jarCanonicalPath, dirNameInJar, zos, true);
            }

            return newJarFile;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    // 记录jar/war文件中的jar文件序号
    private void recordJarInJarNum(String jarCanonicalPath) {
        // 获取jar/war文件中的jar文件
        List<String> jarPathList = getJarPathListInJar(jarCanonicalPath);
        for (String jarPath : jarPathList) {
            String jarMergedPath = JavaCG2JarUtil.mergeOuterInnerJarPath(jarCanonicalPath, jarPath);
            jarPathNumMap.put(jarMergedPath, jarNumCounter.addAndGet());
        }
    }

    /*
        todo
         检查在指定的目录中的-output_javacg2目录、-javacg2_merged.jar文件不会被合并
         若直接指定以上文件，还是需要处理
     */
    // 获取文件或目录列表
    private List<File> getJarFileOrDirList() {
        List<File> jarFileOrDirList = new ArrayList<>(jarOrDirPathList.size());
        Set<String> jarFileOrDirPathSet = new HashSet<>();

        for (String currentJarOrDirPath : jarOrDirPathList) {
            File jarFileOrDir = new File(currentJarOrDirPath);
            String jarDirCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileOrDir);
            if (!jarFileOrDir.exists()) {
                logger.error("{} 配置文件中指定的文件或目录不存在: {}", JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey(), jarDirCanonicalPath);
                return null;
            }
            if (ignoreJarByNamePath(jarDirCanonicalPath)) {
                // 假如指定了合并产生的jar文件，且指定的jar文件数量大于1，则跳过
                logger.info("指定了多个文件或目录，跳过合并产生的jar文件: {}", jarDirCanonicalPath);
                continue;
            }
            if (!jarFileOrDirPathSet.add(jarDirCanonicalPath)) {
                logger.warn("跳过重复的jar文件/目录 {} {}", currentJarOrDirPath, jarDirCanonicalPath);
                continue;
            }
            if (jarFileOrDir.isFile()) {
                String jarFilePathLower = jarDirCanonicalPath.toLowerCase();
                if (!JavaCG2FileUtil.checkJarWarFile(jarFilePathLower)) {
                    logger.error("{} 配置文件中指定文件时只允许指定jar或war文件", JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey());
                    return null;
                }
                if (javaCG2ElManager.checkIgnoreMergeFileInDir(jarDirCanonicalPath)) {
                    logger.debug("合并jar文件时跳过目录中的jar/war文件 {}", jarDirCanonicalPath);
                    continue;
                }
            }
            jarFileOrDirList.add(jarFileOrDir);
            jarPathNumMap.put(jarDirCanonicalPath, jarNumCounter.addAndGet());
        }

        return jarFileOrDirList;
    }

    // 获得新的jar文件文件
    private static File getNewJarFile(File firstJarFile, String firstJarPath) {
        if (firstJarFile.isFile()) {
            // List第一个元素为jar文件
            return new File(firstJarPath + JavaCG2Constants.MERGED_JAR_FLAG);
        }

        // List第一个元素为目录
        return new File(firstJarPath + File.separator + firstJarFile.getName() + JavaCG2Constants.MERGED_JAR_FLAG);
    }

    // 处理jar/war文件，将其中的文件添加到目标jar文件中
    private void handleJarWarFile(File sourceJarFile, String jarWarCanonicalPath, String firstLevelDirName, ZipOutputStream targetZos, boolean handleJarInJar) throws IOException {
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile)))) {
            doHandleJarFile(zipInputStream, jarWarCanonicalPath, firstLevelDirName, targetZos, handleJarInJar);
        }
    }

    private void doHandleJarFile(ZipInputStream zipInputStream, String jarWarCanonicalPath, String firstLevelDirName, ZipOutputStream targetZos, boolean handleJarInJar) throws IOException {
        LocalFileHeader fileHeader;
        while ((fileHeader = zipInputStream.getNextEntry()) != null) {
            if (fileHeader.isDirectory()) {
                continue;
            }

            String jarEntryPath = fileHeader.getFileName();

            // 判断当前文件是否需要跳过
            if (javaCG2ElManager.checkIgnoreMergeFileInJarWar(jarEntryPath)) {
                logger.debug("合并jar文件时跳过jar/war文件中的文件 {} {}", jarWarCanonicalPath, jarEntryPath);
                continue;
            }

            if (JavaCG2FileUtil.checkJarFile(jarEntryPath) && handleJarInJar) {
                // 处理jar/war文件中的jar文件
                // 不能使用try-with-resource，否则流关闭后后续无法再读取
                ZipInputStream innerZipInputStream = new ZipInputStream(new BufferedInputStream(zipInputStream));
                String mergedJarPath = JavaCG2JarUtil.mergeOuterInnerJarPath(jarWarCanonicalPath, jarEntryPath);
                int jarNum = jarPathNumMap.get(mergedJarPath);
                String jarFileName = JavaCG2JarUtil.getJarEntryNameFromPath(jarEntryPath);
                // 获得生成的jar文件中的目录名称
                String dirNameInJar = JavaCG2JarUtil.genDirNameInJar(jarNum, jarFileName);
                doHandleJarFile(innerZipInputStream, null, dirNameInJar, targetZos, false);
            }

            // 处理jar/war文件中的一个文件
            ZipParameters zipParameters = new ZipParameters();
            zipParameters.setFileNameInZip(firstLevelDirName + JavaCG2Constants.FLAG_SLASH + jarEntryPath);
            targetZos.putNextEntry(zipParameters);

            // 向目标jar文件写入数据
            addInput2Jar(zipInputStream, targetZos);
        }
    }

    // 处理目录，将其中的文件添加到目标jar文件中
    private void handlerDir(File sourceDirFile, String firstLevelDirName, List<File> jarFileInDirList, ZipOutputStream targetZos) throws IOException {
        // 保存后缀非.jar文件对象列表
        List<File> nonJarFileList = new ArrayList<>();
        // 保存后缀非.jar文件的相对路径列表
        List<String> nonJarFileRelativelyPathList = new ArrayList<>();

        // 处理指定目录中不同后缀的文件
        handleFileInSubDir(sourceDirFile, null, nonJarFileList, nonJarFileRelativelyPathList, jarFileInDirList);
        if (nonJarFileList.isEmpty()) {
            return;
        }

        for (int i = 0; i < nonJarFileList.size(); i++) {
            String filePath = nonJarFileRelativelyPathList.get(i);
            // 修改写入jar文件中的文件名的第一层目录
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

    // 向目标jar文件写入数据
    private void addInput2Jar(InputStream inputStream, ZipOutputStream targetZos) throws IOException {
        byte[] data = new byte[8192];
        int len;
        while ((len = inputStream.read(data)) > 0) {
            targetZos.write(data, 0, len);
            totalSize += len;
        }
        targetZos.closeEntry();
    }

    // 处理指定目录中不同后缀的文件
    private void handleFileInSubDir(File dirFile, String dirPath, List<File> nonJarFileList, List<String> fileRelativelyPathList, List<File> jarFileInDirList) {
        File[] files = dirFile.listFiles();
        if (files == null) {
            return;
        }

        String dirPathHeader = (dirPath == null ? dirFile.getName() : dirPath + JavaCG2Constants.FLAG_SLASH + dirFile.getName());

        for (File file : files) {
            String fileName = file.getName();
            if (file.isDirectory()) {
                // 递归处理目录
                if (!ignoreDirByName(fileName)) {
                    handleFileInSubDir(file, dirPathHeader, nonJarFileList, fileRelativelyPathList, jarFileInDirList);
                }
                continue;
            }

            // 处理文件
            String fileCanonicalPath = JavaCG2FileUtil.getCanonicalPath(file);
            if (javaCG2ElManager.checkIgnoreMergeFileInDir(fileCanonicalPath)) {
                logger.debug("合并jar文件时跳过目录中的文件 {}", fileCanonicalPath);
                continue;
            }

            if (JavaCG2FileUtil.checkJarWarFile(fileName)) {
                if (!ignoreJarByNamePath(fileName)) {
                    // 目录中的当前文件后缀是jar/war，记录文件对象
                    jarFileInDirList.add(file);
                }
            } else {
                // 目录中的当前文件后缀不是jar/war
                // 记录后缀非.jar文件的文件对象及相对路径
                nonJarFileList.add(file);
                fileRelativelyPathList.add(dirPathHeader + JavaCG2Constants.FLAG_SLASH + fileName);
            }
        }
    }

    // 根据文件名或路径忽略jar文件
    private boolean ignoreJarByNamePath(String jarFileNamePath) {
        return jarFileNamePath.endsWith(JavaCG2Constants.MERGED_JAR_FLAG);
    }

    // 根据目录名忽略目录
    private boolean ignoreDirByName(String dirName) {
        /*
            需要跳过以下目录：
            保存处理失败class文件的子目录
            保存当前组件输出文件的目录
         */
        return JavaCG2DirEnum.IDE_FAIL_CLASSES.getDirName().equals(dirName) || dirName.endsWith(JavaCG2Constants.DIR_TAIL_OUTPUT);
    }
}
