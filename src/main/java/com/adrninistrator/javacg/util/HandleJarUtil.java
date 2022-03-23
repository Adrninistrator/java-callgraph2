package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.JarInfo;

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

    /**
     * 对指定的jar包进行处理
     * 若指定的数组只有一个元素，且为jar包，则直接返回
     * 其他情况下，需要生成新的jar包
     *
     * @param jarPathArray
     * @param jarInfoMap   保存需要处理的jar包文件名及对应的序号，序号从1开始
     * @return null: 处理失败，非null: 新生成的jar包路径，或原有的jar包路径
     */
    public static String handleJar(String[] jarPathArray, Map<String, JarInfo> jarInfoMap) {
        if (jarPathArray.length == 1) {
            // 数组只指定了一个元素
            File oneFile = new File(jarPathArray[0]);
            String oneFilePath = FileUtil.getCanonicalPath(oneFile);

            if (!oneFile.exists()) {
                System.err.println("指定的jar包或目录不存在: " + oneFilePath);
                return null;
            }

            if (oneFile.isFile()) {
                // 指定的是一个jar包，直接返回，向map中保存数据的key使用固定值
                jarInfoMap.put(JavaCGConstants.ONE_JAR_INFO_KEY,
                        JarInfo.genJarInfo(1, JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX, oneFilePath));
                return oneFilePath;
            }
        }

        // 指定的是一个目录，或数组指定了多于一个元素，需要生成新的jar包
        return mergeJar(jarPathArray, jarInfoMap);
    }

    /**
     * 合并jar包
     * 将每个jar包或目录生成一个新的jar包，第一层目录名为原jar包或目录名
     * 若指定的数组第1个元素为jar包，则新生成的jar包生成在同一个目录中
     * 若指定的数组第1个元素为目录，则新生成的jar包生成在该目录中
     * 若只指定了一个目录，也需要生成新的jar包
     *
     * @param jarPathArray
     * @param jarInfoMap
     * @return 合并后的jar包文件路径
     */
    private static String mergeJar(String[] jarPathArray, Map<String, JarInfo> jarInfoMap) {
        // 获取文件列表
        List<File> jarFileList = getJarFileList(jarPathArray, jarInfoMap);
        if (jarFileList == null) {
            return null;
        }

        // 获得新的jar包文件
        File newJarFile = getNewJarFile(jarFileList.get(0), jarPathArray[0]);
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

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(newJarFile))) {
            for (File jarFile : jarFileList) {
                if (jarFile.isFile()) {
                    // 将jar包添加到jar包中
                    addJar2Jar(jarFile, zos);
                    continue;
                }

                // 将目录添加到jar包中
                addDir2Jar(jarFile, zos);
            }

            return FileUtil.getCanonicalPath(newJarFile);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    // 获取文件列表
    private static List<File> getJarFileList(String[] jarPathArray, Map<String, JarInfo> jarInfoMap) {
        Set<String> jarNameSet = new HashSet<>(jarPathArray.length);
        List<File> jarFileList = new ArrayList<>(jarPathArray.length);

        int jarNum = 0;

        for (String currentJarPath : jarPathArray) {
            File jarFile = new File(currentJarPath);
            String jarCanonicalPath = FileUtil.getCanonicalPath(jarFile);
            if (!jarFile.exists()) {
                System.err.println("指定的jar包或目录不存在: " + jarCanonicalPath);
                return null;
            }

            String jarName = jarFile.getName();
            if (jarNameSet.contains(jarName)) {
                System.err.println("指定的jar包或目录存在同名: " + jarName);
                return null;
            }

            jarNameSet.add(jarName);
            jarFileList.add(jarFile);

            String jarType = jarFile.isFile() ? JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX : JavaCGConstants.FILE_KEY_DIR_INFO_PREFIX;
            jarInfoMap.put(jarName, JarInfo.genJarInfo(++jarNum, jarType, jarCanonicalPath));
        }
        return jarFileList;
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

    // 将jar包添加到jar包中
    private static void addJar2Jar(File sourceJarFile, ZipOutputStream targetZos) throws IOException {
        String sourceJarName = sourceJarFile.getName();

        try (JarFile jar = new JarFile(sourceJarFile)) {
            Enumeration<JarEntry> enumeration = jar.entries();
            while (enumeration.hasMoreElements()) {
                JarEntry jarEntry = enumeration.nextElement();
                if (jarEntry.isDirectory() || jarEntry.getName().toLowerCase().endsWith(JavaCGConstants.EXT_JAR)) {
                    // 跳过目录，或jar文件
                    continue;
                }

                // 处理jar包中的一个非jar文件
                ZipEntry newZipEntry = new ZipEntry(sourceJarName + "/" + jarEntry.getName());
                targetZos.putNextEntry(newZipEntry);

                try (InputStream inputStream = jar.getInputStream(jarEntry)) {
                    // 向目标jar文件写入数据
                    addInput2Jar(inputStream, targetZos);
                }
            }
        }
    }

    // 将目录添加到jar包中
    private static void addDir2Jar(File sourceDirFile, ZipOutputStream targetZos) throws IOException {
        List<File> fileList = new ArrayList<>();
        List<String> fileRelativelyPathList = new ArrayList<>();
        // 查找指定目录下，排除指定类型文件的文件列表
        FileUtil.findFileInSubDirExclude(sourceDirFile, null, JavaCGConstants.EXT_JAR, fileList, fileRelativelyPathList);
        if (fileList.isEmpty()) {
            System.err.println("从目录中未找到指定类型外的文件: " + FileUtil.getCanonicalPath(sourceDirFile) + " " + JavaCGConstants.EXT_JAR);
            return;
        }

        for (int i = 0; i < fileList.size(); i++) {
            ZipEntry newZipEntry = new ZipEntry(fileRelativelyPathList.get(i));
            targetZos.putNextEntry(newZipEntry);

            // 向目标jar文件写入数据
            try (InputStream inputStream = new FileInputStream(fileList.get(i))) {
                addInput2Jar(inputStream, targetZos);
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
