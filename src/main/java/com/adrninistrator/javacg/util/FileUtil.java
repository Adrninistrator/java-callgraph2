package com.adrninistrator.javacg.util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

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

    private FileUtil() {
        throw new IllegalStateException("illegal");
    }
}
