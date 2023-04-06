package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import org.apache.commons.lang3.StringUtils;

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
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author adrninistrator
 * @date 2022/2/8
 * @description:
 */
public class JavaCGJarUtil {

    // 获取合并jar/war包中的class文件时，需要合并的特定包名
    private static Set<String> getMergeClassInJarPackageSet(Set<String> needHandlePackageSet) {
        if (JavaCGUtil.isCollectionEmpty(needHandlePackageSet)) {
            return new HashSet<>();
        }

        Set<String> mergeClassInJarPackageSet = new HashSet<>(needHandlePackageSet.size());

        System.out.println("合并jar/war包中的class文件时，需要合并的包名:");
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

            System.out.println(newMergeClassInJarPackage);

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
     * @param jarInfoMap           保存需要处理的jar包文件名及对应的序号，序号从1开始
     * @param needHandlePackageSet
     * @return null: 处理失败，非null: 新生成的jar包文件，或原有的jar包文件
     */
    public static File handleJar(List<String> jarOrDirPathList, Map<String, JarInfo> jarInfoMap, Set<String> needHandlePackageSet) {
        if (jarOrDirPathList.size() == 1) {
            // 数组只指定了一个元素
            File oneFile = new File(jarOrDirPathList.get(0));
            String oneFilePath = JavaCGFileUtil.getCanonicalPath(oneFile);

            if (!oneFile.exists()) {
                System.err.println("指定的jar包或目录不存在: " + oneFilePath);
                return null;
            }

            if (oneFile.isFile()) {
                String oneFileNameLower = oneFile.getName().toLowerCase(Locale.ROOT);
                if (!StringUtils.endsWithAny(oneFileNameLower, JavaCGConstants.EXT_JAR, JavaCGConstants.EXT_WAR)) {
                    System.err.println("处理单个文件时只支持指定" + JavaCGConstants.EXT_JAR + "或" + JavaCGConstants.EXT_WAR + "格式，假如需要处理" + JavaCGConstants.EXT_CLASS + "格式的文件，则需要指定其所在目录");
                    return null;
                }

                // 指定的是一个jar包，直接返回
                // 记录jar包信息，向map中保存数据的key使用固定值
                jarInfoMap.put(oneFile.getName(), new JarInfo(JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, oneFilePath));
                return oneFile;
            }
        }

        // 指定的是一个目录，或数组指定了多于一个元素，需要生成新的jar包
        return mergeJar(jarOrDirPathList, jarInfoMap, needHandlePackageSet);
    }

    /**
     * 合并jar包
     * 将每个jar包或目录生成一个新的jar包，第一层目录名为原jar包或目录名
     * 若指定的数组第1个元素为jar包，则新生成的jar包生成在同一个目录中
     * 若指定的数组第1个元素为目录，则新生成的jar包生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar包
     *
     * @param jarOrDirPathList
     * @param jarInfoMap
     * @param needHandlePackageSet
     * @return 合并后的jar包文件路径
     */
    private static File mergeJar(List<String> jarOrDirPathList, Map<String, JarInfo> jarInfoMap, Set<String> needHandlePackageSet) {
        // 获取文件或目录列表
        List<File> jarFileOrDirList = getJarFileOrDirList(jarOrDirPathList, jarInfoMap);
        if (jarFileOrDirList == null) {
            return null;
        }

        // 获得新的jar包文件
        File newJarFile = getNewJarFile(jarFileOrDirList.get(0), jarOrDirPathList.get(0));
        if (newJarFile.exists()) {
            // 新的jar包文件已存在
            if (newJarFile.isDirectory()) {
                System.err.println("新的jar包文件已存在，但是是目录: " + JavaCGFileUtil.getCanonicalPath(newJarFile));
                return null;
            } else if (!JavaCGFileUtil.deleteFile(newJarFile)) {
                System.err.println("新的jar包文件已存在，删除失败: " + JavaCGFileUtil.getCanonicalPath(newJarFile));
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
                    System.err.println("指定的jar/war包或目录存在同名，不处理: " + jarFileOrDirName + " " + JavaCGFileUtil.getCanonicalPath(jarFileOrDir));
                    continue;
                }
                destJarDirNameSet.add(jarFileOrDirName);

                if (jarFileOrDir.isFile()) {
                    // 将jar/war包添加到jar包中
                    addJar2Jar(jarFileOrDir, zos, mergeClassInJarPackageSet);
                    continue;
                }

                // 将目录添加到jar包中
                addDir2Jar(jarFileOrDir, jarFileInDirList, zos);
            }

            // 合并目录中的后缀为.jar/.war文件
            for (File jarFileInDir : jarFileInDirList) {
                String jarFileName = jarFileInDir.getName();
                String jarCanonicalPath = JavaCGFileUtil.getCanonicalPath(jarFileInDir);
                // 记录jar包信息，不覆盖现有值
                jarInfoMap.putIfAbsent(jarFileName, new JarInfo(JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, jarCanonicalPath));

                if (destJarDirNameSet.contains(jarFileName)) {
                    System.err.println("指定的jar包或目录存在同名，不处理: " + jarFileName + " " + jarCanonicalPath);
                    continue;
                }
                destJarDirNameSet.add(jarFileName);

                System.out.println("添加目录中的jar/war包: " + jarCanonicalPath);
                // 将jar/war包添加到jar包中
                addJar2Jar(jarFileInDir, zos, mergeClassInJarPackageSet);
            }

            return newJarFile;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 获取文件或目录列表
     *
     * @param jarOrDirPathList
     * @param jarInfoMap
     * @return
     */
    private static List<File> getJarFileOrDirList(List<String> jarOrDirPathList, Map<String, JarInfo> jarInfoMap) {
        List<File> jarFileOrDirList = new ArrayList<>(jarOrDirPathList.size());

        for (String currentJarOrDirPath : jarOrDirPathList) {
            File jarFileOrDir = new File(currentJarOrDirPath);
            String jarCanonicalPath = JavaCGFileUtil.getCanonicalPath(jarFileOrDir);
            if (!jarFileOrDir.exists()) {
                System.err.println("指定的jar包或目录不存在: " + jarCanonicalPath);
                return null;
            }

            jarFileOrDirList.add(jarFileOrDir);

            String jarOrDirType = jarFileOrDir.isFile() ? JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX : JavaCGConstants.FILE_KEY_DIR_INFO_PREFIX;
            // 记录jar包信息，不覆盖现有值
            jarInfoMap.putIfAbsent(jarFileOrDir.getName(), new JarInfo(jarOrDirType, jarCanonicalPath));
        }

        return jarFileOrDirList;
    }

    // 获得新的jar包文件
    private static File getNewJarFile(File firstJarFile, String firstJarPath) {
        if (firstJarFile.isFile()) {
            // 数组第1个元素为jar包
            return new File(firstJarPath + JavaCGConstants.MERGED_JAR_FLAG);
        }

        // 数组第1个元素为目录
        return new File(firstJarPath + File.separator + firstJarFile.getName() + JavaCGConstants.MERGED_JAR_FLAG);
    }

    // 将jar/war包添加到jar包中
    private static void addJar2Jar(File sourceJarFile, ZipOutputStream targetZos, Set<String> mergeClassInJarPackageSet) throws IOException {
        String sourceJarName = sourceJarFile.getName();

        try (JarInputStream jarInputStream = new JarInputStream(new BufferedInputStream(new FileInputStream(sourceJarFile), 1024 * 32))) {
            JarEntry jarEntry;
            while ((jarEntry = jarInputStream.getNextJarEntry()) != null) {
                if (jarEntry.isDirectory()) {
                    // 跳过目录
                    continue;
                }

                String jarEntryName = jarEntry.getName();
                String jarEntryNameLower = jarEntryName.toLowerCase();
                if (!jarEntryNameLower.endsWith(JavaCGConstants.EXT_CLASS)) {
                    // 跳过非.class文件
                    continue;
                }

                if (skipClassEntry(jarEntryName, mergeClassInJarPackageSet)) {
                    // 跳过class文件
                    continue;
                }

                // 处理jar包中的一个非jar文件
                ZipEntry newZipEntry = new ZipEntry(sourceJarName + "/" + jarEntryName);
                targetZos.putNextEntry(newZipEntry);

                // 向目标jar文件写入数据
                addInput2Jar(jarInputStream, targetZos);
            }
        }
    }

    /**
     * 是否跳过class文件
     *
     * @param jarEntryName
     * @param mergeClassInJarPackageSet
     * @return true: 跳过 false: 不跳过
     */
    private static boolean skipClassEntry(String jarEntryName, Set<String> mergeClassInJarPackageSet) {
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
            if (jarEntryName.startsWith(mergeClassInJarPackage) ||
                    jarEntryName.startsWith("WEB-INF/classes/" + mergeClassInJarPackage) ||
                    jarEntryName.startsWith("BOOT-INF/classes/" + mergeClassInJarPackage)
            ) {
                if (JavaCGLogUtil.isDebugPrintFlag()) {
                    JavaCGLogUtil.debugPrint("当前class文件包名匹配，需要合并 " + jarEntryName + " " + mergeClassInJarPackage);
                }
                // 不跳过包名满足要求的class文件
                return false;
            }
        }

        // 跳过包名不满足要求的class文件
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("当前class文件包名不匹配，不合并 " + jarEntryName);
        }
        return true;
    }

    // 将目录添加到jar包中
    private static void addDir2Jar(File sourceDirFile, List<File> jarFileInDirList, ZipOutputStream targetZos) throws IOException {
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
            ZipEntry newZipEntry = new ZipEntry(nonJarFileRelativelyPathList.get(i));
            targetZos.putNextEntry(newZipEntry);

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
            String currentFileNameLower = currentFileName.toLowerCase();
            if (currentFileNameLower.endsWith(JavaCGConstants.EXT_JAR) || currentFileNameLower.endsWith(JavaCGConstants.EXT_WAR)) {
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
                fileRelativelyPathList.add(dirPathHeader + "/" + currentFileNameLower);
            }
        }
    }

    // 向目标jar文件写入数据
    private static void addInput2Jar(InputStream inputStream, ZipOutputStream targetZos) throws IOException {
        byte[] data = new byte[4096];
        int len;
        while ((len = inputStream.read(data)) > 0) {
            targetZos.write(data, 0, len);
        }
    }

    private JavaCGJarUtil() {
        throw new IllegalStateException("illegal");
    }
}
