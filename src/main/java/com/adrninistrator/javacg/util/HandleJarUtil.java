package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.jar.JarInfo;

import java.io.*;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author adrninistrator
 * @date 2022/2/8
 * @description:
 */
public class HandleJarUtil {

    // 获取合并jar/war包中的class文件时，需要合并的特定包名
    private static List<String> getMergeClassInJarPackageList() {
        List<String> mergeClassInJarPackageList = new ArrayList<>();
        String mergeClassInJarPackageProperty = System.getProperty(JavaCGConstants.PROPERTY_MERGE_CLASS_IN_JAR_PACKAGE);
        if (mergeClassInJarPackageProperty == null || mergeClassInJarPackageProperty.trim().isEmpty()) {
            System.out.println("合并jar/war包中的class文件时，全部都进行合并，不区分包名");
            return mergeClassInJarPackageList;
        }

        String[] mergeClassInJarPackageArray = mergeClassInJarPackageProperty.split(JavaCGConstants.FLAG_HASHTAG);
        System.out.println("合并jar/war包中的class文件时，需要合并的包名");
        for (String mergeClassInJarPackage : mergeClassInJarPackageArray) {
            if (mergeClassInJarPackage.isEmpty()) {
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
                if (newMergeClassInJarPackage.isEmpty()) {
                    continue;
                }
            }
            if (!newMergeClassInJarPackage.endsWith("/")) {
                newMergeClassInJarPackage += "/";
            }

            System.out.println(newMergeClassInJarPackage);

            mergeClassInJarPackageList.add(newMergeClassInJarPackage);
        }

        return mergeClassInJarPackageList;
    }

    /**
     * 对指定的jar包进行处理
     * 若指定的数组只有一个元素，且为jar包，则直接返回
     * 其他情况下，需要生成新的jar包
     *
     * @param jarOrDirPathArray
     * @param jarInfoMap        保存需要处理的jar包文件名及对应的序号，序号从1开始
     * @return null: 处理失败，非null: 新生成的jar包路径，或原有的jar包路径
     */
    public static String handleJar(String[] jarOrDirPathArray, Map<String, JarInfo> jarInfoMap) {
        if (jarOrDirPathArray.length == 1) {
            // 数组只指定了一个元素
            File oneFile = new File(jarOrDirPathArray[0]);
            String oneFilePath = FileUtil.getCanonicalPath(oneFile);

            if (!oneFile.exists()) {
                System.err.println("指定的jar包或目录不存在: " + oneFilePath);
                return null;
            }

            if (oneFile.isFile()) {
                // 指定的是一个jar包，直接返回
                // 记录jar包信息，向map中保存数据的key使用固定值
                jarInfoMap.put(oneFile.getName(), JarInfo.genJarInfo(JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, oneFilePath));
                return oneFilePath;
            }
        }

        // 指定的是一个目录，或数组指定了多于一个元素，需要生成新的jar包
        return mergeJar(jarOrDirPathArray, jarInfoMap);
    }

    /**
     * 合并jar包
     * 将每个jar包或目录生成一个新的jar包，第一层目录名为原jar包或目录名
     * 若指定的数组第1个元素为jar包，则新生成的jar包生成在同一个目录中
     * 若指定的数组第1个元素为目录，则新生成的jar包生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar包
     *
     * @param jarOrDirPathArray
     * @param jarInfoMap
     * @return 合并后的jar包文件路径
     */
    private static String mergeJar(String[] jarOrDirPathArray, Map<String, JarInfo> jarInfoMap) {
        // 获取文件或目录列表
        List<File> jarFileOrDirList = getJarFileOrDirList(jarOrDirPathArray, jarInfoMap);
        if (jarFileOrDirList == null) {
            return null;
        }

        // 获得新的jar包文件
        File newJarFile = getNewJarFile(jarFileOrDirList.get(0), jarOrDirPathArray[0]);
        if (newJarFile.exists()) {
            // 新的jar包文件已存在
            if (newJarFile.isDirectory()) {
                System.err.println("新的jar包文件已存在，但是是目录: " + FileUtil.getCanonicalPath(newJarFile));
                return null;
            } else if (!FileUtil.deleteFile(newJarFile)) {
                System.err.println("新的jar包文件已存在，删除失败: " + FileUtil.getCanonicalPath(newJarFile));
                return null;
            }
        }

        // 已添加到目标jar包中的目录名称
        Set<String> destJarDirNameSet = new HashSet<>(jarOrDirPathArray.length);

        // 目录中的jar包文件对象列表
        List<File> jarFileInDirList = new ArrayList<>();

        // 获取合并jar/war包中的class文件时，需要合并的特定包名
        List<String> mergeClassInJarPackageList = getMergeClassInJarPackageList();

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(newJarFile))) {
            // 合并参数中指定的jar包，以及目录中的后缀非.jar/.war文件
            for (File jarFileOrDir : jarFileOrDirList) {
                String jarFileOrDirName = jarFileOrDir.getName();
                if (destJarDirNameSet.contains(jarFileOrDirName)) {
                    System.err.println("指定的jar/war包或目录存在同名，不处理: " + jarFileOrDirName + " " + FileUtil.getCanonicalPath(jarFileOrDir));
                    continue;
                }
                destJarDirNameSet.add(jarFileOrDirName);

                if (jarFileOrDir.isFile()) {
                    // 将jar/war包添加到jar包中
                    addJar2Jar(jarFileOrDir, zos, mergeClassInJarPackageList);
                    continue;
                }

                // 将目录添加到jar包中
                addDir2Jar(jarFileOrDir, jarFileInDirList, zos);
            }

            // 合并目录中的后缀为.jar/.war文件
            for (File jarFileInDir : jarFileInDirList) {
                String jarFileName = jarFileInDir.getName();
                String jarCanonicalPath = FileUtil.getCanonicalPath(jarFileInDir);
                // 记录jar包信息，不覆盖现有值
                jarInfoMap.putIfAbsent(jarFileName, JarInfo.genJarInfo(JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, jarCanonicalPath));

                if (destJarDirNameSet.contains(jarFileName)) {
                    System.err.println("指定的jar包或目录存在同名，不处理: " + jarFileName + " " + jarCanonicalPath);
                    continue;
                }
                destJarDirNameSet.add(jarFileName);

                System.out.println("添加目录中的jar/war包: " + jarCanonicalPath);
                // 将jar/war包添加到jar包中
                addJar2Jar(jarFileInDir, zos, mergeClassInJarPackageList);
            }

            return FileUtil.getCanonicalPath(newJarFile);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 获取文件或目录列表
     *
     * @param jarOrDirPathArray
     * @param jarInfoMap
     * @return
     */
    private static List<File> getJarFileOrDirList(String[] jarOrDirPathArray, Map<String, JarInfo> jarInfoMap) {
        List<File> jarFileOrDirList = new ArrayList<>(jarOrDirPathArray.length);

        for (String currentJarOrDirPath : jarOrDirPathArray) {
            File jarFileOrDir = new File(currentJarOrDirPath);
            String jarCanonicalPath = FileUtil.getCanonicalPath(jarFileOrDir);
            if (!jarFileOrDir.exists()) {
                System.err.println("指定的jar包或目录不存在: " + jarCanonicalPath);
                return null;
            }

            jarFileOrDirList.add(jarFileOrDir);

            String jarOrDirType = jarFileOrDir.isFile() ? JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX : JavaCGConstants.FILE_KEY_DIR_INFO_PREFIX;
            // 记录jar包信息，不覆盖现有值
            jarInfoMap.putIfAbsent(jarFileOrDir.getName(), JarInfo.genJarInfo(jarOrDirType, jarCanonicalPath));
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
    private static void addJar2Jar(File sourceJarFile, ZipOutputStream targetZos, List<String> mergeClassInJarPackageList) throws IOException {
        String sourceJarName = sourceJarFile.getName();

        try (JarFile jar = new JarFile(sourceJarFile)) {
            Enumeration<JarEntry> enumeration = jar.entries();
            while (enumeration.hasMoreElements()) {
                JarEntry jarEntry = enumeration.nextElement();
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

                if (!mergeClassInJarPackageList.isEmpty()) {
                    // 根据class文件包名进行处理
                    boolean classPackageMatches = false;

                    for (String mergeClassInJarPackage : mergeClassInJarPackageList) {
                        if (jarEntryName.startsWith(mergeClassInJarPackage) || jarEntryName.startsWith("WEB-INF/classes/" + mergeClassInJarPackage)) {
                            classPackageMatches = true;
                            if (JavaCGUtil.enableDebugPrint()) {
                                JavaCGUtil.debugPrint("当前class文件包名匹配，需要合并 " + jarEntryName + " " + mergeClassInJarPackage);
                            }
                            break;
                        }
                    }

                    if (!classPackageMatches) {
                        // 跳过包名不满足要求的class文件
                        if (JavaCGUtil.enableDebugPrint()) {
                            JavaCGUtil.debugPrint("当前class文件包名不匹配，不合并 " + jarEntryName);
                        }
                        continue;
                    }
                }

                // 处理jar包中的一个非jar文件
                ZipEntry newZipEntry = new ZipEntry(sourceJarName + "/" + jarEntryName);
                targetZos.putNextEntry(newZipEntry);

                try (InputStream inputStream = jar.getInputStream(jarEntry)) {
                    // 向目标jar文件写入数据
                    addInput2Jar(inputStream, targetZos);
                }
            }
        }
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

    private HandleJarUtil() {
        throw new IllegalStateException("illegal");
    }
}
