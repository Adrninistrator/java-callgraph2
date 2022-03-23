package com.adrninistrator.javacg.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class FileUtil {

    public static String getCanonicalPath(String filePath) {
        try {
            return new File(filePath).getCanonicalPath();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static String getCanonicalPath(File file) {
        try {
            return file.getCanonicalPath();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static boolean deleteFile(File file) {
        try {
            Files.delete(file.toPath());
            return true;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * 查找指定目录下，排除指定类型文件的文件列表
     *
     * @param dirFile
     * @param dirPath
     * @param excludeFileExt
     * @param resultFileList         记录查找到的文件列表
     * @param fileRelativelyPathList 记录查找到的文件相对路径列表
     */
    public static void findFileInSubDirExclude(File dirFile, String dirPath, String excludeFileExt, List<File> resultFileList, List<String> fileRelativelyPathList) {
        File[] files = dirFile.listFiles();
        if (files == null) {
            return;
        }

        String dirPathHeader = (dirPath == null ? dirFile.getName() : dirPath + "/" + dirFile.getName());

        for (File file : files) {
            if (file.isDirectory()) {
                findFileInSubDirExclude(file, dirPathHeader, excludeFileExt, resultFileList, fileRelativelyPathList);
            } else {
                String currentFileName = file.getName();
                if (!currentFileName.toLowerCase().endsWith(excludeFileExt)) {
                    resultFileList.add(file);
                    fileRelativelyPathList.add(dirPathHeader + "/" + currentFileName);
                }
            }
        }
    }

    private FileUtil() {
        throw new IllegalStateException("illegal");
    }
}
