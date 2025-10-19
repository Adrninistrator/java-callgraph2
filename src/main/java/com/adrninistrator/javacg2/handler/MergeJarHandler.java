package com.adrninistrator.javacg2.handler;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.file.FilePathWithName;
import com.adrninistrator.javacg2.dto.jar.MergeJarResult;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2JarUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.adrninistrator.javacg2.util.RunProcessUtil;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/1/25
 * @description: 用于合并jar、war、jmod、class文件的处理类
 */
public class MergeJarHandler {

    private static final Logger logger = LoggerFactory.getLogger(MergeJarHandler.class);

    private static final String[] ALLOWED_FILE_TYPES = new String[]{
            JavaCG2Constants.EXT_JAR, JavaCG2Constants.EXT_WAR, JavaCG2Constants.EXT_JMOD,
    };

    private static final String ALLOWED_FILE_TYPE_STR = StringUtils.join(ALLOWED_FILE_TYPES, " ");

    // 普通的jar、war、目录等路径列表
    private final List<String> normalFileDirPathList = new ArrayList<>();

    // 找到的jmod文件路径列表
    private final List<FilePathWithName> jmodFilePathWithNameList = new ArrayList<>();

    // jmod文件解压后的路径列表
    private final List<String> jmodExtractDirPathList = new ArrayList<>();

    private final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    private final List<String> jarOrDirPathList;

    private final Map<String, Integer> jarPathNumMap;

    private final JavaCG2Counter jarNumCounter;

    private final JavaCG2ElManager javaCG2ElManager;

    // 是否只需要获取输出文件路径
    private final boolean onlyForOutputFile;

    private final String outputRootPath;

    private final boolean mergeSeparateFatJar;

    private final int jdkRuntimeMajorVersion;

    private long totalSize = 0;

    /**
     * @param javaCG2ConfigureWrapper 需要合并的jar、war、jmod文件，或目录的列表
     * @param jarPathNumMap           保存jar文件路径与对应序号的Map
     * @param jarNumCounter           jar文件序号计数器
     * @param javaCG2ElManager        表达式管理类
     * @param onlyForOutputFile       是否仅用于获取输出文件路径
     */
    public MergeJarHandler(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, Map<String, Integer> jarPathNumMap, JavaCG2Counter jarNumCounter, JavaCG2ElManager javaCG2ElManager,
                           boolean onlyForOutputFile) {
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
        jarOrDirPathList = javaCG2ConfigureWrapper.getOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR);
        if (jarOrDirPathList.isEmpty()) {
            logger.error("请在配置文件中指定需要解析的jar文件或目录列表 {}", javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR));
            throw new JavaCG2RuntimeException("未指定需要解析的jar文件或目录列表");
        }
        this.jarPathNumMap = jarPathNumMap;
        this.jarNumCounter = jarNumCounter;
        this.javaCG2ElManager = javaCG2ElManager;
        this.onlyForOutputFile = onlyForOutputFile;

        outputRootPath = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH);
        mergeSeparateFatJar = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_MERGE_SEPARATE_FAT_JAR);
        jdkRuntimeMajorVersion = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_JDK_RUNTIME_MAJOR_VERSION);

        if (!onlyForOutputFile) {
            logger.info("需要解析的jar文件或目录:\n{}", StringUtils.join(jarOrDirPathList, "\n"));
        }
    }

    /**
     * 合并指定的jar、war、jmod文件或目录
     * 将指定的jar、war、jmod文件或目录生成一个新的jar文件，第一层目录名为原jar、war、jmod文件或目录名
     * 若指定的jarOrDirPathList第一个元素为jar、war、jmod文件，则新生成的jar文件生成在同一个目录中
     * 若指定的jarOrDirPathList第一个元素为目录，则新生成的jar文件生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar文件
     *
     * @return null: 处理失败，非null: 新生成的jar文件，或原有的jar文件
     */
    public MergeJarResult mergeJar() {
        File mergeFile = null;
        boolean onlyOneJar = false;
        if (jarOrDirPathList.size() == 1) {
            // List只指定了一个元素
            String jarOrDirPath = jarOrDirPathList.get(0);
            mergeFile = new File(jarOrDirPath);
            String oneFilePath = JavaCG2FileUtil.getCanonicalPath(mergeFile);

            if (!mergeFile.exists()) {
                logger.error("指定了一个jar文件或目录，不存在: {}", oneFilePath);
                return null;
            }

            if (mergeFile.isFile()) {
                if (!JavaCG2FileUtil.checkJarWarJmodFile(oneFilePath)) {
                    logger.error("处理单个文件时只支持指定 {} 格式，假如需要处理 {} 格式的文件，则需要指定其所在目录", ALLOWED_FILE_TYPE_STR, JavaCG2Constants.EXT_CLASS);
                    return null;
                }

                // 检查是否仅处理一个jar文件，不需要进行合并
                if (checkOnlyHandleOneJarFile(oneFilePath)) {
                    // 仅处理一个jar文件，不需要进行合并
                    if (!onlyForOutputFile) {
                        // 不是只需要获取输出文件路径，指定的jar文件中不存在jar文件，记录jar文件信息
                        int jarNum = jarNumCounter.addAndGet();
                        // 记录jar、war文件对应的序号
                        jarPathNumMap.put(oneFilePath, jarNum);
                    }
                    logger.info("仅指定了一个jar、war文件，直接使用 {} {}", jarOrDirPath, oneFilePath);
                    onlyOneJar = true;
                }
            }
        }

        long startTime = System.currentTimeMillis();
        if (!onlyOneJar) {
            // 需要合并生成jar文件
            mergeFile = doMergeJar();
            if (mergeFile == null) {
                return null;
            }
        }

        if (onlyForOutputFile) {
            // 只需要获取输出文件路径，后续操作不需要执行
            return MergeJarResult.genOne(mergeFile);
        }
        if (!mergeSeparateFatJar) {
            // 不需要合并fat jar
            String fileSizeDisplay = FileUtils.byteCountToDisplaySize(totalSize);
            logger.info("合并jar文件完毕，耗时 {} 秒，（解压后）总大小约 {}", JavaCG2Util.getSpendSeconds(startTime), fileSizeDisplay);
            return MergeJarResult.genOne(mergeFile);
        }
        // 需要合并fat jar
        File fatJar = mergeFatJar(mergeFile);
        if (fatJar == null) {
            return null;
        }
        return MergeJarResult.genTwo(mergeFile, fatJar);
    }

    /**
     * 检查是否仅处理一个jar文件，不需要进行合并
     *
     * @param oneFilePath
     * @return false: 需要进行合并 true: 不需要进行合并
     */
    private boolean checkOnlyHandleOneJarFile(String oneFilePath) {
        if (JavaCG2FileUtil.checkJmodFile(oneFilePath)) {
            logger.info(".jmod文件，需要进行合并 {}", oneFilePath);
            return false;
        }
        // 指定的是一个jar、war文件，判断其中是否有jar文件
        List<String> jarPathList = getJarPathListInJar(oneFilePath);
        if (!jarPathList.isEmpty()) {
            logger.info("jar文件中有jar文件，需要进行合并 {}", oneFilePath);
            return false;
        }

        // 检查是否为 multi-release JAR 文件
        if (checkMultiReleaseJarFile(oneFilePath)) {
            logger.info("属于 multi-release JAR ，需要进行合并 {}", oneFilePath);
            return false;
        }
        logger.info("仅指定了一个jar、war文件，不需要进行合并 {}", oneFilePath);
        return true;
    }

    /**
     * 获取jar、war文件中的jar文件路径列表
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
            logger.error("判断jar、war文件中是否存在jar文件异常 {} ", jarFilePath, e);
            return Collections.emptyList();
        }
    }

    /**
     * 执行合并生成jar文件
     *
     * @return 合并后的jar文件路径
     */
    private File doMergeJar() {
        // 获得新生成的jar文件
        File newJarFile = getNewJarFile(false);
        if (onlyForOutputFile) {
            // 只需要获取输出文件路径，直接返回新生成的jar文件，不需要执行合并操作
            return newJarFile;
        }

        if (newJarFile.exists()) {
            // 新的jar文件已存在
            if (newJarFile.isDirectory()) {
                logger.error("新的jar文件已存在，但是是目录: {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
                return null;
            } else if (!JavaCG2FileUtil.deleteFile(newJarFile)) {
                logger.error("新的jar文件已存在，删除失败: {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
                return null;
            }
        } else if (!JavaCG2FileUtil.isDirectoryExists(newJarFile.getParentFile(), true)) {
            logger.error("创建新的jar文件所在目录失败 {}", JavaCG2FileUtil.getCanonicalPath(newJarFile));
            return null;
        }

        // 预处理需要解析的文件与目录
        if (!preHandleJarOrDirPathList()) {
            return null;
        }

        // 处理jmod文件
        if (!handleJmodFiles()) {
            return null;
        }

        // 目录中的jar文件对象列表
        List<File> jarFileInDirList = new ArrayList<>();

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(newJarFile))) {
            // 处理配置参数中指定的需要解析的jar、war文件，以及目录中的后缀非jar、war、jmod文件
            for (String normalFileDirPath : normalFileDirPathList) {
                File jarFileOrDir = new File(normalFileDirPath);
                // 获得生成的jar文件中的目录名称
                String dirNameInJar = genDirNameInJar(jarFileOrDir);
                if (jarFileOrDir.isFile()) {
                    // 处理jar、war文件，将其中的文件添加到目标jar文件中
                    if (!handleJarWarFile(jarFileOrDir, normalFileDirPath, dirNameInJar, zos)) {
                        return null;
                    }
                    continue;
                }

                // 检查是否为解压jmod文件的目录
                if (!JavaCG2Constants.DIR_NAME_JAVACG2_JMOD_EXTRACT.equals(jarFileOrDir.getName())) {
                    // 处理目录，将其中的文件添加到目标jar文件中
                    handlerDir(jarFileOrDir, dirNameInJar, jarFileInDirList, zos);
                }
            }

            // 合并从目录中找到的后缀为jar、war的文件
            for (File jarFileInDir : jarFileInDirList) {
                String jarCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileInDir);
                if (jarPathNumMap.containsKey(jarCanonicalPath)) {
                    // 避免在jar_dir配置文件中指定了目录与jar文件时，目录中包含对应jar文件时重复处理对应jar文件
                    logger.warn("目录中的jar、war文件在处理对应目录时已处理过，不再处理 {}", jarCanonicalPath);
                    continue;
                }

                int jarNum = jarNumCounter.addAndGet();
                // 记录jar、war文件对应的序号
                jarPathNumMap.put(jarCanonicalPath, jarNum);

                // 记录jar、war文件中的jar文件序号
                recordJarInJarNum(jarCanonicalPath);

                // 获得生成的jar文件中的目录名称
                String dirNameInJar = JavaCG2JarUtil.genDirNameInJar(jarNum, jarFileInDir.getName());

                logger.info("处理目录中的jar、war文件: {}", jarCanonicalPath);
                // 处理jar、war文件，将其中的文件添加到目标jar文件中
                if (!handleJarWarFile(jarFileInDir, jarCanonicalPath, dirNameInJar, zos)) {
                    return null;
                }
            }

            // 处理.jmod文件解压出来的目录，将其中的文件添加到目标jar文件中
            for (String jmodExtractDirPath : jmodExtractDirPathList) {
                File jmodExtractDir = new File(jmodExtractDirPath);
                // 获得生成的jar文件中的目录名称
                String dirNameInJar = genDirNameInJar(jmodExtractDir);
                handlerDir(jmodExtractDir, dirNameInJar, jarFileInDirList, zos);
            }

            return newJarFile;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    // 获得生成的jar文件中的目录名称
    private String genDirNameInJar(File dir) {
        String dirCanonicalPath = JavaCG2FileUtil.getCanonicalPath(dir);
        int jarNum = jarPathNumMap.get(dirCanonicalPath);
        return JavaCG2JarUtil.genDirNameInJar(jarNum, dir.getName());
    }

    // 记录jar、war文件中的jar文件序号
    private void recordJarInJarNum(String jarCanonicalPath) {
        // 获取jar、war文件中的jar文件
        List<String> jarPathList = getJarPathListInJar(jarCanonicalPath);
        for (String jarPath : jarPathList) {
            String jarMergedPath = JavaCG2JarUtil.mergeOuterInnerJarPath(jarCanonicalPath, jarPath);
            // 记录jar、war文件对应的序号
            jarPathNumMap.put(jarMergedPath, jarNumCounter.addAndGet());
        }
    }

    // 预处理需要解析的文件与目录
    private boolean preHandleJarOrDirPathList() {
        // 遍历需要解析的文件与目录
        for (String currentJarOrDirPath : jarOrDirPathList) {
            File jarFileOrDir = new File(currentJarOrDirPath);
            String jarDirCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jarFileOrDir);
            if (!jarFileOrDir.exists()) {
                logger.error("配置文件中指定的需要解析的文件或目录不存在 {} {}", jarDirCanonicalPath, javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR));
                return false;
            }
            if (jarFileOrDir.isFile()) {
                // 预处理需要解析的文件
                if (!preHandleFile(jarDirCanonicalPath)) {
                    return false;
                }
            } else {
                // 预处理需要解析的目录
                if (!preHandleDir(jarDirCanonicalPath)) {
                    return false;
                }
            }
        }
        return true;
    }

    // 预处理需要解析的文件
    private boolean preHandleFile(String fileCanonicalPath) {
        if (javaCG2ElManager.checkIgnoreMergeFileInDir(fileCanonicalPath)) {
            logger.info("合并jar文件时，通过表达式判断需要跳过指定的文件 {}", fileCanonicalPath);
            return true;
        }

        if (JavaCG2FileUtil.checkJmodFile(fileCanonicalPath)) {
            // 尝试添加需要解析的.jmod文件路径
            return tryAddJmodFilePath(fileCanonicalPath);
        }

        if (JavaCG2FileUtil.checkJarWarFile(fileCanonicalPath)) {
            if (ignoreJarByNamePath(fileCanonicalPath)) {
                // 假如指定了合并产生的jar文件，且指定的jar文件数量大于1，则跳过
                logger.info("指定了多个文件或目录，跳过合并产生的jar文件: {}", fileCanonicalPath);
                return true;
            }
            if (!normalFileDirPathList.contains(fileCanonicalPath)) {
                // 找到jar、war文件，且未记录过，则进行记录
                normalFileDirPathList.add(fileCanonicalPath);
                // 记录jar、war文件对应的序号
                jarPathNumMap.put(fileCanonicalPath, jarNumCounter.addAndGet());
            } else {
                logger.warn("跳过重复的jar、文件 {}", fileCanonicalPath);
            }
            return true;
        }
        logger.error("在配置文件中指定的需要解析的文件非法 {} 只允许指定 {} {}", fileCanonicalPath, ALLOWED_FILE_TYPES,
                javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR));
        return false;
    }

    // 预处理需要解析的目录
    private boolean preHandleDir(String dirCanonicalPath) {
        if (normalFileDirPathList.contains(dirCanonicalPath)) {
            // 已记录过的目录，跳过
            logger.warn("跳过重复的目录 {}", dirCanonicalPath);
            return true;
        }
        normalFileDirPathList.add(dirCanonicalPath);
        // 记录目录对应的序号
        jarPathNumMap.put(dirCanonicalPath, jarNumCounter.addAndGet());
        // 在指定的目录中查找.jmod文件
        List<String> subFilePathList = new ArrayList<>();
        JavaCG2FileUtil.searchDir(dirCanonicalPath, null, subFilePathList, JavaCG2Constants.EXT_JMOD);
        if (subFilePathList.isEmpty()) {
            return true;
        }
        for (String subFilePath : subFilePathList) {
            if (javaCG2ElManager.checkIgnoreMergeFileInDir(subFilePath)) {
                logger.info("合并jar文件时，通过表达式判断需要跳过指定目录中的jmod文件 {}", subFilePath);
                continue;
            }
            // 尝试添加需要解析的.jmod文件路径
            if (!tryAddJmodFilePath(subFilePath)) {
                return false;
            }
        }
        return true;
    }

    // 处理jmod文件
    private boolean handleJmodFiles() {
        if (jmodFilePathWithNameList.isEmpty()) {
            logger.info("未找到jmod文件，不需要处理");
            return true;
        }
        logger.info("找到 {} 个jmod文件，开始处理", jmodFilePathWithNameList.size());

        if (StringUtils.isBlank(outputRootPath)) {
            logger.error("需要处理jmod文件，缺少配置参数 {}", javaCG2ConfigureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH));
            return false;
        }

        String jmodProgramPath = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_JMOD_PROGRAM_PATH);
        if (StringUtils.isNotBlank(jmodProgramPath)) {
            if (!JavaCG2FileUtil.isFileExists(jmodProgramPath)) {
                logger.error("配置参数指定的jmod程序不存在 {} {}", jmodProgramPath, javaCG2ConfigureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH));
                return false;
            }
        } else {
            // 通过需要解析的jmod文件查找jmod程序
            jmodProgramPath = getJmodProgramPathFromJmodFile();
            if (jmodProgramPath == null) {
                logger.error("通过需要解析的jmod文件未找到jmod程序，需要通过配置参数指定的jmod程序路径 {}", javaCG2ConfigureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH));
                return false;
            }
        }

        // 解压.jmod文件
        return extractJmodFiles(jmodProgramPath);
    }

    // 解压.jmod文件
    private boolean extractJmodFiles(String jmodProgramPath) {
        String jmodFileExtractRootDirPath = JavaCG2FileUtil.addSeparator4FilePath(outputRootPath) + JavaCG2Constants.DIR_NAME_JAVACG2_JMOD_EXTRACT;
        if (!JavaCG2FileUtil.isDirectoryExists(jmodFileExtractRootDirPath)) {
            logger.error("创建解压.jmod文件的目录失败 {}", jmodFileExtractRootDirPath);
            return false;
        }

        for (FilePathWithName jmodFilePathWithName : jmodFilePathWithNameList) {
            String jmodExtractDirPath = jmodFileExtractRootDirPath + File.separator + jmodFilePathWithName.getFileName();
            if (JavaCG2FileUtil.isFileExists(jmodExtractDirPath)) {
                logger.error(".jmod文件需要解压的目录存在同名文件 {}", jmodExtractDirPath);
                return false;
            }

            if (JavaCG2FileUtil.isDirectoryExists(jmodExtractDirPath, false)) {
                // 需要解压的目录已存在，删除
                logger.info("删除已存在的.jmod文件需要解压的目录 {}", jmodExtractDirPath);
                try {
                    FileUtils.deleteDirectory(new File(jmodExtractDirPath));
                } catch (Exception e) {
                    logger.error("删除.jmod文件需要解压的已存在的目录失败 {}", jmodExtractDirPath);
                    return false;
                }
            }
            logger.info("使用jmod程序 [{}] 解压.jmod文件 [{}] 到目录 [{}]", jmodProgramPath, jmodFilePathWithName.getFilePath(), jmodExtractDirPath);
            String[] args = new String[]{
                    RunProcessUtil.handleProcessArg(jmodProgramPath),
                    "extract",
                    "--dir",
                    RunProcessUtil.handleProcessArg(jmodExtractDirPath),
                    RunProcessUtil.handleProcessArg(jmodFilePathWithName.getFilePath())
            };
            if (!RunProcessUtil.runProcess(args)) {
                logger.error("解压.jmod文件失败 {}", jmodFilePathWithName.getFilePath());
                return false;
            }

            // 检查解压后的classes目录
            String jmodExtractClassesDirPath = jmodExtractDirPath + File.separator + JavaCG2Constants.DIR_NAME_CLASSES;
            if (!JavaCG2FileUtil.isDirectoryExists(jmodExtractClassesDirPath, false)) {
                logger.error("解压.jmod文件后的classes目录不存在 {} {}", jmodFilePathWithName.getFilePath(), jmodExtractClassesDirPath);
                return false;
            }
            String jmodExtractClassesDirCanonicalPath = JavaCG2FileUtil.getCanonicalPath(jmodExtractClassesDirPath);
            // 记录jmod文件解压后的路径
            jmodExtractDirPathList.add(jmodExtractClassesDirCanonicalPath);
            // 记录jar、war文件对应的序号
            jarPathNumMap.put(jmodExtractClassesDirCanonicalPath, jarNumCounter.addAndGet());
        }

        return true;
    }

    // 通过需要解析的jmod文件查找jmod程序
    private String getJmodProgramPathFromJmodFile() {
        boolean windowsOs = JavaCG2Util.checkWindowsOs();
        for (FilePathWithName jmodFilePathWithName : jmodFilePathWithNameList) {
            File jmodFile = new File(jmodFilePathWithName.getFilePath());
            String jmodProgramPath = jmodFile.getParent() + (windowsOs ? "\\..\\bin\\jmod.exe" : "/../bin/jmod");
            if (JavaCG2FileUtil.isFileExists(jmodProgramPath)) {
                logger.info("通过需要解析的jmod文件找到jmod程序 {} {}", jmodFilePathWithName.getFilePath(), jmodProgramPath);
                return JavaCG2FileUtil.getCanonicalPath(jmodProgramPath);
            }
        }
        return null;
    }

    // 获得新的jar文件
    private File getNewJarFile(boolean mergeFatJar) {
        String jarFlag = mergeFatJar ? JavaCG2Constants.MERGED_FAT_JAR_FLAG : JavaCG2Constants.MERGED_JAR_FLAG;
        // 获得参数中指定的需要解析的第一个文件，文件路径使用绝对路径
        File firstJarFile = new File(JavaCG2FileUtil.getCanonicalPath(jarOrDirPathList.get(0)));
        String rootDirPath;
        if (StringUtils.isNotBlank(outputRootPath)) {
            // 若有指定output.root.path参数，则生成在对应目录中
            rootDirPath = JavaCG2FileUtil.addSeparator4FilePath(outputRootPath);
            return new File(rootDirPath + firstJarFile.getName() + jarFlag);
        }

        // 默认生成在第一个jar、war文件，或目录所在的目录
        rootDirPath = JavaCG2FileUtil.addSeparator4FilePath(firstJarFile.getParent());
        if (firstJarFile.isFile()) {
            // List第一个元素为jar文件
            return new File(rootDirPath + firstJarFile.getName() + jarFlag);
        }
        // List第一个元素为目录
        return new File(rootDirPath + firstJarFile.getName() + File.separator + firstJarFile.getName() + jarFlag);
    }

    // 处理jar、war文件，将其中的文件添加到目标jar文件中
    private boolean handleJarWarFile(File sourceJarFile, String jarWarCanonicalPath, String firstLevelDirName, ZipOutputStream targetZos) throws IOException {
        if (javaCG2ElManager.checkNeedIgnoreJarWarByClassDirPrefix()) {
            // 通过class文件对应指定层级的目录路径判断是否跳过合并当前的jar、war文件
            try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile)))) {
                if (checkIgnoreJarWarByClassDirPrefix(zipInputStream)) {
                    logger.info("忽略合并当前jar、war文件 {}", sourceJarFile.getAbsolutePath());
                    return true;
                }
            }
        }
        // 记录jar、war文件中的jar文件序号
        recordJarInJarNum(jarWarCanonicalPath);

        Set<String> loadClassFilePathWithPrefixSet;
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile)))) {
            // 处理 multi-release JAR 文件
            loadClassFilePathWithPrefixSet = handleMultiReleaseJarFile(zipInputStream, jarWarCanonicalPath, "");
        }

        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile)))) {
            // 执行处理jar、war文件
            doHandleJarWarFile(zipInputStream, jarWarCanonicalPath, firstLevelDirName, targetZos, true, loadClassFilePathWithPrefixSet);
            return true;
        }
    }

    /**
     * 执行处理jar、war文件
     *
     * @param zipInputStream
     * @param jarWarCanonicalPath
     * @param firstLevelDirName
     * @param targetZos
     * @param handleJarInJar                 是否需要处理jar文件中的jar文件
     * @param loadClassFilePathWithPrefixSet
     * @throws IOException
     */
    private void doHandleJarWarFile(ZipInputStream zipInputStream, String jarWarCanonicalPath, String firstLevelDirName, ZipOutputStream targetZos,
                                    boolean handleJarInJar, Set<String> loadClassFilePathWithPrefixSet) throws IOException {
        LocalFileHeader fileHeader;
        while ((fileHeader = zipInputStream.getNextEntry()) != null) {
            if (fileHeader.isDirectory()) {
                continue;
            }

            String jarEntryPath = fileHeader.getFileName();
            // 判断当前文件是否需要跳过
            if (javaCG2ElManager.checkIgnoreMergeFileInJarWar(jarEntryPath)) {
                if (JavaCG2FileUtil.checkJarFile(jarEntryPath) && handleJarInJar) {
                    // 跳过处理jar、war文件中的jar文件
                    ignoreJarInJarWar(jarWarCanonicalPath, jarEntryPath);
                } else if (logger.isDebugEnabled() || javaCG2ElManager.isDebugMode()) {
                    logger.info("合并jar文件时跳过jar、war文件中的文件 {} {}", jarWarCanonicalPath, jarEntryPath);
                }
                continue;
            }

            if (JavaCG2FileUtil.checkJarFile(jarEntryPath) && handleJarInJar) {
                // 处理jar、war文件中的jar文件
                // 将不可重复读的ZipInputStream缓存为可以重复读取的ByteArrayInputStream
                InputStream cachedInputStream = JavaCG2Util.cacheInputStream(zipInputStream);
                if (cachedInputStream == null) {
                    logger.error("缓存jar文件内容失败 {}", jarEntryPath);
                    throw new JavaCG2RuntimeException("缓存jar文件内容失败");
                }

                // 判断是否需要通过class文件对应指定层级的目录路径判断是否跳过合并当前的jar/war文件
                if (javaCG2ElManager.checkNeedIgnoreJarWarByClassDirPrefix()) {
                    ZipInputStream innerZipInputStream = new ZipInputStream(cachedInputStream);
                    // 通过class文件对应指定层级的目录路径判断是否跳过合并当前的jar、war文件
                    if (checkIgnoreJarWarByClassDirPrefix(innerZipInputStream)) {
                        // 跳过处理jar、war文件中的jar文件
                        ignoreJarInJarWar(jarWarCanonicalPath, jarEntryPath);
                        continue;
                    }
                    // 重置缓存的InputStream，使下次能够从头开始继续读取
                    cachedInputStream.reset();
                }

                // 不能使用try-with-resource，否则流关闭后后续无法再读取
                ZipInputStream innerZipInputStream = new ZipInputStream(cachedInputStream);
                // 处理jar文件中的 multi-release JAR 文件
                Set<String> innerLoadClassFilePathWithPrefixSet = handleMultiReleaseJarFile(innerZipInputStream, jarWarCanonicalPath, jarEntryPath);

                String mergedJarPath = JavaCG2JarUtil.mergeOuterInnerJarPath(jarWarCanonicalPath, jarEntryPath);
                int jarNum = jarPathNumMap.get(mergedJarPath);
                String jarFileName = JavaCG2JarUtil.getJarEntryNameFromPath(jarEntryPath);
                // 获得生成的jar文件中的目录名称
                String dirNameInJar = JavaCG2JarUtil.genDirNameInJar(jarNum, jarFileName);

                // 重置缓存的InputStream，使下次能够从头开始继续读取
                cachedInputStream.reset();
                ZipInputStream innerZipInputStream2 = new ZipInputStream(cachedInputStream);
                // 执行处理jar、war文件，递归调用，处理jar文件中的jar文件
                doHandleJarWarFile(innerZipInputStream2, null, dirNameInJar, targetZos, false, innerLoadClassFilePathWithPrefixSet);
                continue;
            }
            // 判断是否为.class文件，且是Multi-release JAR中不需要使用的类
            if (JavaCG2FileUtil.checkClassFile(jarEntryPath) && !loadClassFilePathWithPrefixSet.isEmpty() && !loadClassFilePathWithPrefixSet.contains(jarEntryPath)) {
                if (logger.isDebugEnabled()) {
                    logger.debug("当前Multi-release JAR的.class文件不需要使用 {}", jarEntryPath);
                }
                continue;
            }

            // 处理jar、war文件中的一个非jar文件
            ZipParameters zipParameters = new ZipParameters();
            zipParameters.setFileNameInZip(firstLevelDirName + JavaCG2Constants.FLAG_SLASH + jarEntryPath);
            targetZos.putNextEntry(zipParameters);

            // 向目标jar文件写入数据
            addInput2Jar(zipInputStream, targetZos);
        }
    }

    /**
     * 检查是否为 multi-release JAR 文件
     *
     * @return false: 不是 multi-release JAR 文件 true: 是 multi-release JAR 文件
     */
    private boolean checkMultiReleaseJarFile(String jarPath) {
        LocalFileHeader fileHeader;
        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(jarPath)))) {
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                if (!fileHeader.isDirectory()) {
                    if (JavaCG2Constants.META_INF_MANIFEST.equals(fileHeader.getFileName())) {
                        // 找到 META-INF/MANIFEST.MF 文件，判断是否为Multi-release JAR
                        return JavaCG2JarUtil.checkMultiReleaseJar(zipInputStream);
                    }
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
            throw new JavaCG2RuntimeException("检查是否为 multi-release JAR 文件出现异常");
        }
        return false;
    }

    /**
     * 处理 multi-release JAR 文件
     *
     * @param zipInputStream
     * @return multi-release JAR 需要加载的class文件路径集合，若为空则代表全部需要加载
     * @return jarPath jar文件路径
     * @return innerJarPath jar文件中的jar文件路径
     * @throws IOException
     */
    private Set<String> handleMultiReleaseJarFile(ZipInputStream zipInputStream, String jarPath, String innerJarPath) throws IOException {
        // jar文件根目录的class文件路径
        Set<String> classInJarRootPathSet = new HashSet<>();
        /*
            jar文件 META-INF/versions/ 目录的class文件路径
         */
        Map<Integer, Set<String>> classInJarMetaInfVersionsPathMap = new HashMap<>();

        boolean multiReleaseJar = false;
        LocalFileHeader fileHeader;
        while ((fileHeader = zipInputStream.getNextEntry()) != null) {
            if (fileHeader.isDirectory()) {
                continue;
            }
            String fileName = fileHeader.getFileName();
            if (JavaCG2Constants.META_INF_MANIFEST.equals(fileName)) {
                // 找到 META-INF/MANIFEST.MF 文件，判断是否为Multi-release JAR
                multiReleaseJar = JavaCG2JarUtil.checkMultiReleaseJar(zipInputStream);
                if (!multiReleaseJar) {
                    // 假如不是Multi-release JAR，则返回需要加载的class文件路径为空，代表class文件全部都需要加载
                    logger.info("有找到 {} 文件，jar文件需要按照 Multi-release JAR 方式处理 [{}] [{}]", JavaCG2Constants.META_INF_MANIFEST, jarPath, innerJarPath);
                    return Collections.emptySet();
                }
                continue;
            }
            if (!fileName.endsWith(JavaCG2Constants.EXT_CLASS)) {
                // 跳过非.class文件
                continue;
            }
            // 处理.class文件
            if (fileName.startsWith(JavaCG2Constants.META_INF_VERSIONS)) {
                // .class文件在 META-INF/versions/ 目录中
                int indexOfFirstSlash = fileName.indexOf(JavaCG2Constants.CHAR_SLASH, JavaCG2Constants.META_INF_VERSIONS_LENGTH);
                String versionStr = fileName.substring(JavaCG2Constants.META_INF_VERSIONS_LENGTH, indexOfFirstSlash);
                // 获取版本号
                int version = Integer.parseInt(versionStr);
                // 获取去掉版本号前缀的.class文件路径
                String classPath = fileName.substring(indexOfFirstSlash + JavaCG2Constants.SLASH_LENGTH);
                Set<String> classInJarMetaInfVersionsPathSet = classInJarMetaInfVersionsPathMap.computeIfAbsent(version, k -> new HashSet<>());
                classInJarMetaInfVersionsPathSet.add(classPath);
                continue;
            }
            // .class文件在根目录
            classInJarRootPathSet.add(fileName);
        }

        if (!multiReleaseJar) {
            // 假如不是Multi-release JAR，则返回需要加载的class文件路径为空，代表class文件全部都需要加载
            logger.info("未找到 {} 文件，jar文件需要按照 Multi-release JAR 方式处理 [{}] [{}]", JavaCG2Constants.META_INF_MANIFEST, jarPath, innerJarPath);
            return Collections.emptySet();
        }

        logger.info("jar文件需要按照 Multi-release JAR 方式处理 [{}] [{}]", jarPath, innerJarPath);
        // 根据类加载顺序，获取Multi-release JAR需要加载的.class文件路径
        return getMultiReleaseJarLoadClasses(classInJarRootPathSet, classInJarMetaInfVersionsPathMap);
    }

    // 根据类加载顺序，获取Multi-release JAR需要加载的.class文件路径
    private Set<String> getMultiReleaseJarLoadClasses(Set<String> classInJarRootPathSet, Map<Integer, Set<String>> classInJarMetaInfVersionsPathMap) {
        // 保存需要加载的class文件路径，包含 META-INF/versions/ 目录前缀
        Set<String> loadClassFilePathWithPrefixSet = new HashSet<>();
        if (jdkRuntimeMajorVersion == 8) {
            // JDK8及以下版本，只使用根目录中的.class文件
            loadClassFilePathWithPrefixSet.addAll(classInJarRootPathSet);
            return loadClassFilePathWithPrefixSet;
        }
        /*
            JDK9及以上版本处理
            类加载顺序
            - 优先查找 N 版本化目录下的类文件
            - 若未找到，则按版本号降序检查更早的版本化目录（下限为版本 9）
            - 最后回退到顶级目录
         */
        // 保存已处理的class文件路径，不包含前缀
        Set<String> handledClassFilePathSet = new HashSet<>();
        // 处理 META-INF/versions/ 目录中的版本号，使用小于等于运行时JDK版本，之后按版本号从高到低排序
        List<Integer> versionList = new ArrayList<>();
        for (Integer version : classInJarMetaInfVersionsPathMap.keySet()) {
            if (version <= jdkRuntimeMajorVersion) {
                versionList.add(jdkRuntimeMajorVersion);
            }
        }
        versionList.sort(Collections.reverseOrder());

        // 根据优先级处理不同版本的 META-INF/versions/ 目录中的.class文件
        for (Integer version : versionList) {
            Set<String> classInJarMetaInfVersionsPathSet = classInJarMetaInfVersionsPathMap.get(version);
            if (classInJarMetaInfVersionsPathSet == null) {
                // 运行时的JDK版本可能在目录中不存在，跳过
                continue;
            }
            for (String classInJarMetaInfVersionsPath : classInJarMetaInfVersionsPathSet) {
                if (handledClassFilePathSet.add(classInJarMetaInfVersionsPath)) {
                    // 若.class文件路径未处理过，则记录为需要加载
                    loadClassFilePathWithPrefixSet.add(genClassPathInMetaInfVersions(version, classInJarMetaInfVersionsPath));
                }
            }
        }
        // 处理根目录的.class文件
        for (String classInJarRootPath : classInJarRootPathSet) {
            if (handledClassFilePathSet.add(classInJarRootPath)) {
                // 若.class文件路径未处理过，则记录为需要加载
                loadClassFilePathWithPrefixSet.add(classInJarRootPath);
            }
        }
        return loadClassFilePathWithPrefixSet;
    }

    // 生成 META-INF/versions/ 目录中的class文件路径
    private String genClassPathInMetaInfVersions(int version, String classPath) {
        return JavaCG2Constants.META_INF_VERSIONS + version + JavaCG2Constants.FLAG_SLASH + classPath;
    }

    // 跳过处理jar、war文件中的jar文件
    private void ignoreJarInJarWar(String jarWarCanonicalPath, String jarEntryPath) {
        logger.info("合并jar文件时跳过jar、war文件中的jar文件 {} {}", jarWarCanonicalPath, jarEntryPath);
        String jarMergedPath = JavaCG2JarUtil.mergeOuterInnerJarPath(jarWarCanonicalPath, jarEntryPath);
        // 当前jar、war文件中的jar文件不处理，需要从记录jar文件序号的Map中删除
        jarPathNumMap.remove(jarMergedPath);
    }

    // 通过class文件对应指定层级的目录路径判断是否跳过合并当前的jar、war文件
    private boolean checkIgnoreJarWarByClassDirPrefix(ZipInputStream zipInputStream) throws IOException {
        Map<Integer, Set<String>> classDirPrefixMap = new HashMap<>();
        LocalFileHeader fileHeader;
        while ((fileHeader = zipInputStream.getNextEntry()) != null) {
            if (fileHeader.isDirectory()) {
                continue;
            }

            String jarEntryPath = fileHeader.getFileName();
            if (!JavaCG2FileUtil.checkClassFile(jarEntryPath)) {
                continue;
            }
            // 获取class文件所在目录路径
            String classFileRelativelyPath = JavaCG2FileUtil.getClassFileRelativelyPathInJar(jarEntryPath);
            String classFileDirPath = JavaCG2FileUtil.getFileDirPathSupportSlash(classFileRelativelyPath);
            String[] classFileDirPathArray = classFileDirPath.split(JavaCG2Constants.FLAG_SLASH);
            // 记录class文件目录指定层级的路径
            for (int i = classFileDirPathArray.length; i >= 1; i--) {
                String classFileDirPathPrefix = StringUtils.join(classFileDirPathArray, JavaCG2Constants.FLAG_SLASH, 0, i);
                Set<String> classFileDirPathPrefixSet = classDirPrefixMap.computeIfAbsent(i, k -> new HashSet<>());
                if (!classFileDirPathPrefixSet.add(classFileDirPathPrefix)) {
                    // 若数值更大的层级对应的class文件目录前缀已记录，则说明数值更低的层级也已经记录，可以结束循环
                    break;
                }
            }
        }
        return javaCG2ElManager.checkIgnoreJarWarByClassDirPrefix(classDirPrefixMap);
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

    /**
     * 尝试添加需要解析的.jmod文件路径
     *
     * @param jmodFilePath
     * @return
     */
    private boolean tryAddJmodFilePath(String jmodFilePath) {
        File jmodFile = new File(jmodFilePath);
        String jmodFileName = jmodFile.getName();
        for (FilePathWithName filePathWithName : jmodFilePathWithNameList) {
            if (filePathWithName.getFilePath().equals(jmodFilePath)) {
                logger.warn("跳过重复的jmod文件 {}", jmodFilePath);
                return true;
            }
            if (filePathWithName.getFileName().equals(jmodFileName)) {
                logger.error("指定了文件路径不同但文件名相同的.jmod文件，在解压时会被覆盖，需要修改其中的一个.jmod文件名 {} {}", filePathWithName.getFilePath(), jmodFilePath);
                return false;
            }
        }

        FilePathWithName newFilePathWithName = new FilePathWithName();
        newFilePathWithName.setFilePath(jmodFilePath);
        newFilePathWithName.setFileName(jmodFileName);
        jmodFilePathWithNameList.add(newFilePathWithName);
        return true;
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
            if (JavaCG2FileUtil.checkJarWarFile(fileName)) {
                // 目录中的当前文件后缀是jar、war
                if (ignoreJarByNamePath(fileName)) {
                    // 忽略文件
                    logger.info("合并jar文件时，忽略当前文件 {}", fileCanonicalPath);
                    continue;
                }
            }

            if (javaCG2ElManager.checkIgnoreMergeFileInDir(fileCanonicalPath)) {
                if (logger.isDebugEnabled() || javaCG2ElManager.isDebugMode()) {
                    logger.info("合并jar文件时，通过表达式判断需要跳过指定目录中的文件 {}", fileCanonicalPath);
                }
                continue;
            }

            if (JavaCG2FileUtil.checkJarWarFile(fileName)) {
                // 目录中的当前文件后缀是jar、war，记录文件对象
                jarFileInDirList.add(file);
            } else if (!JavaCG2FileUtil.checkJmodFile(fileName)) {
                // 目录中的当前文件后缀不是jar、war、jmod，记录文件对象及相对路径
                nonJarFileList.add(file);
                fileRelativelyPathList.add(dirPathHeader + JavaCG2Constants.FLAG_SLASH + fileName);
            }
        }
    }

    // 合并fat jar
    private File mergeFatJar(File mergeJarFile) {
        // 获得需要生成的fat jar文件，固定生成在参数中指定的第一个元素
        File fatJarFile = getNewJarFile(true);
        if (fatJarFile.exists()) {
            // 新的jar文件已存在
            if (fatJarFile.isDirectory()) {
                logger.error("fat jar文件已存在，但是是目录: {}", JavaCG2FileUtil.getCanonicalPath(fatJarFile));
                return null;
            } else if (!JavaCG2FileUtil.deleteFile(fatJarFile)) {
                logger.error("fat jar文件已存在，删除失败: {}", JavaCG2FileUtil.getCanonicalPath(fatJarFile));
                return null;
            }
        }

        try (ZipInputStream zipInputStream = new ZipInputStream(new BufferedInputStream(new FileInputStream(mergeJarFile)));
             ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(fatJarFile))) {
            LocalFileHeader fileHeader;
            while ((fileHeader = zipInputStream.getNextEntry()) != null) {
                if (fileHeader.isDirectory()) {
                    continue;
                }

                String jarEntryPath = fileHeader.getFileName();
                if (JavaCG2FileUtil.checkClassFile(jarEntryPath)) {
                    // 处理jar、war文件中的一个class文件，获得fat jar中class文件路径
                    String fatJarClassFilePath = genFatJarClassFilePath(jarEntryPath);
                    ZipParameters zipParameters = new ZipParameters();
                    zipParameters.setFileNameInZip(fatJarClassFilePath);
                    zos.putNextEntry(zipParameters);

                    // 向目标jar文件写入数据
                    addInput2Jar(zipInputStream, zos);
                }
            }

            return fatJarFile;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    // 获得fat jar中class文件路径
    private String genFatJarClassFilePath(String jarEntryPath) {
        String classFilePath = jarEntryPath;
        String firstLetter = jarEntryPath.substring(0, 1);
        if (JavaCG2Util.isNumStr(firstLetter)) {
            // class文件路径以数字开头，说明为合并后的第一层级目录，需要跳过
            classFilePath = StringUtils.substringAfter(jarEntryPath, JavaCG2Constants.FLAG_SLASH);
        }
        if (classFilePath.startsWith(JavaCG2Constants.WEB_INF_CLASSES)) {
            // class文件路径以 WEB-INF/classes/开头，去掉前缀
            classFilePath = StringUtils.substringAfter(classFilePath, JavaCG2Constants.WEB_INF_CLASSES);
        }
        if (classFilePath.startsWith(JavaCG2Constants.BOOT_INF_CLASSES)) {
            // class文件路径以 BOOT-INF/classes/开头，去掉前缀
            classFilePath = StringUtils.substringAfter(classFilePath, JavaCG2Constants.BOOT_INF_CLASSES);
        }
        return classFilePath;
    }

    // 根据文件名或路径忽略jar文件
    private boolean ignoreJarByNamePath(String jarFileNamePath) {
        return StringUtils.endsWithAny(jarFileNamePath, JavaCG2Constants.MERGED_JAR_FLAG, JavaCG2Constants.MERGED_FAT_JAR_FLAG);
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
