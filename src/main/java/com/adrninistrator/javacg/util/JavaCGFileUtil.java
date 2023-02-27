package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class JavaCGFileUtil {

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
     * 将结果写到文件中，使用TAB作为分隔符，在后面增加换行符
     *
     * @param writer
     * @param data
     * @throws IOException
     */
    public static void write2FileWithTab(Writer writer, String... data) throws IOException {
        writer.write(StringUtils.join(data, JavaCGConstants.FILE_COLUMN_SEPARATOR) + JavaCGConstants.NEW_LINE);
    }

    /**
     * 将结果写到文件中，不在后面增加换行符
     *
     * @param writer
     * @param data
     * @throws IOException
     */
    public static void write2FileNoLF(Writer writer, String data) throws IOException {
        writer.write(data);
    }

    /**
     * 判断文件名是否为class文件
     *
     * @param fileName
     * @return
     */
    public static boolean isClassFile(String fileName) {
        if (fileName == null) {
            return false;
        }
        return fileName.toLowerCase().endsWith(JavaCGConstants.EXT_CLASS);
    }

    /**
     * 判断目录是否存在，不存在时尝试创建
     *
     * @param dirPath 需要判断的目录路径
     * @param tryMake 是否尝试创建目录
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(String dirPath, boolean tryMake) {
        File file = new File(dirPath);
        if (file.exists()) {
            if (file.isDirectory()) {
                return true;
            }

            System.err.println("已存在文件: " + dirPath);
            return false;
        }

        if (!tryMake) {
            return false;
        }

        // 目录不存在，则尝试创建
        if (file.mkdirs()) {
            System.out.println("创建目录: " + dirPath);
            return true;
        }

        System.err.println("创建目录失败: " + dirPath);
        return false;
    }

    /**
     * 根据文件路径生成Writer对象
     *
     * @param filePath
     * @return
     */
    public static BufferedWriter genBufferedWriter(String filePath) throws FileNotFoundException {
        return new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filePath), StandardCharsets.UTF_8));
    }

    public static File findFile(String filePath) {
        // 尝试通过文件路径获取文件
        File file = new File(filePath);
        if (file.exists()) {
            System.out.println("通过文件路径获取文件 " + filePath);
            return file;
        }

        // 尝试从classpath中获取文件，路径以/开头
        URL url = JavaCGFileUtil.class.getResource("/" + filePath);
        if (url != null && "file".equals(url.getProtocol())) {
            /*
                当URL中的protocol为"file"时，说明对应的资源为独立文件的形式
                若为"jar"则说明对应的资源是jar包中的文件，不能通过以下方式处理
             */
            System.out.println("从classpath中获取文件 " + url);
            try {
                return new File(url.toURI());
            } catch (Exception e) {
                e.printStackTrace();
                return null;
            }
        }

        return null;
    }

    public static InputStream getFileInputStream(String filePath) throws FileNotFoundException {
        File file = findFile(filePath);
        if (file != null) {
            return new FileInputStream(file);
        }

        /*
            尝试从jar包中读取，路径需要以/开头，从根目录读取，路径中的分隔符需要为/
            不能使用以下方式获取File对象
                new File(xxx.class.getResource("path“).toURI())
            否则会出现异常
                java.lang.IllegalArgumentException: URI is not hierarchical
         */
        InputStream inputStream = JavaCGFileUtil.class.getResourceAsStream("/" + filePath);
        if (inputStream == null) {
            System.err.println("未找到文件 " + filePath);
            throw new JavaCGRuntimeException("未找到文件 " + filePath);
        }

        System.out.println("从jar包中获取文件 " + JavaCGFileUtil.class.getResource("/" + filePath));
        return inputStream;
    }

    /**
     * 读取文件内容到Set中，忽略以#开关的行
     *
     * @param filePath 文件路径
     * @return
     */
    public static Set<String> readFile2Set(String filePath) {
        return readFile2Set(filePath, JavaCGConstants.FLAG_HASHTAG);
    }

    /**
     * 读取文件内容到Set中
     *
     * @param filePath     文件路径
     * @param ignorePrefix 每行需要忽略的前缀，可为null
     * @return
     */
    public static Set<String> readFile2Set(String filePath, String ignorePrefix) {
        try (BufferedReader br = genBufferedReader(getFileInputStream(filePath))) {
            Set<String> set = new HashSet<>();
            String line;
            boolean checkIgnore = StringUtils.isNotBlank(ignorePrefix);
            while ((line = br.readLine()) != null) {
                if (StringUtils.isNotBlank(line)) {
                    if (checkIgnore && line.startsWith(ignorePrefix)) {
                        continue;
                    }

                    set.add(line);
                }
            }
            return set;
        } catch (Exception e) {
            System.err.println("处理文件出现异常 " + filePath);
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 读取文件内容到List中，忽略以#开关的行
     *
     * @param filePath 文件路径
     * @return
     */
    public static List<String> readFile2List(String filePath) {
        return readFile2List(filePath, JavaCGConstants.FLAG_HASHTAG);
    }

    /**
     * 读取文件内容到List中
     *
     * @param filePath     文件路径
     * @param ignorePrefix 每行需要忽略的前缀，可为null
     * @return
     */
    public static List<String> readFile2List(String filePath, String ignorePrefix) {
        try (BufferedReader br = genBufferedReader(getFileInputStream(filePath))) {
            List<String> list = new ArrayList<>();
            String line;
            boolean checkIgnore = StringUtils.isNotBlank(ignorePrefix);
            while ((line = br.readLine()) != null) {
                if (StringUtils.isNotBlank(line)) {
                    if (checkIgnore && line.startsWith(ignorePrefix)) {
                        continue;
                    }

                    list.add(line);
                }
            }
            return list;
        } catch (Exception e) {
            System.err.println("处理文件出现异常 " + filePath);
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 获取文件的BufferedReader
     *
     * @param filePath
     * @return
     * @throws FileNotFoundException
     */
    public static BufferedReader genBufferedReader(String filePath) throws FileNotFoundException {
        return new BufferedReader(new InputStreamReader(new FileInputStream(filePath), StandardCharsets.UTF_8));
    }

    /**
     * 获取文件的BufferedReader
     *
     * @param file
     * @return
     * @throws FileNotFoundException
     */
    public static BufferedReader genBufferedReader(File file) throws FileNotFoundException {
        return new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8));
    }

    /**
     * 获取InputStream的BufferedReader
     *
     * @param input
     * @return
     * @throws FileNotFoundException
     */
    public static BufferedReader genBufferedReader(InputStream input) {
        return new BufferedReader(new InputStreamReader(input, StandardCharsets.UTF_8));
    }

    private JavaCGFileUtil() {
        throw new IllegalStateException("illegal");
    }
}
