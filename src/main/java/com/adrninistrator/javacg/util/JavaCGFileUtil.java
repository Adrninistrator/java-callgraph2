package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
import java.nio.file.FileAlreadyExistsException;
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

    private static final Logger logger = LoggerFactory.getLogger(JavaCGFileUtil.class);

    public static String getCanonicalPath(String filePath) {
        return getCanonicalPath(new File(filePath));
    }

    public static String getCanonicalPath(File file) {
        try {
            return file.getCanonicalPath();
        } catch (IOException e) {
            logger.error("error ", e);
            return null;
        }
    }

    public static boolean deleteFile(File file) {
        try {
            Files.delete(file.toPath());
            return true;
        } catch (IOException e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 拼接需要写入文件的各列数据
     *
     * @param data
     * @return
     */
    public static String appendFileColumn(String... data) {
        return StringUtils.join(data, JavaCGConstants.FLAG_TAB);
    }

    /**
     * 将结果写到文件中，使用TAB作为分隔符，在后面增加换行符
     *
     * @param writer
     * @param data
     * @throws IOException
     */
    public static void write2FileWithTab(Writer writer, String... data) throws IOException {
        writer.write(appendFileColumn(data) + JavaCGConstants.NEW_LINE);
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
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(String dirPath) {
        return isDirectoryExists(new File(dirPath), true);
    }

    /**
     * 判断目录是否存在
     *
     * @param dirPath 需要判断的目录路径
     * @param tryMake 不存在时是否尝试创建
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(String dirPath, boolean tryMake) {
        return isDirectoryExists(new File(dirPath), tryMake);
    }

    /**
     * 判断目录是否存在，不存在时尝试创建
     *
     * @param dirFile 需要判断的目录对象
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(File dirFile) {
        return isDirectoryExists(dirFile, true);
    }

    /**
     * 判断目录是否存在
     *
     * @param dirFile 需要判断的目录对象
     * @param tryMake 不存在时是否尝试创建
     * @return 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(File dirFile, boolean tryMake) {
        if (dirFile.exists()) {
            if (dirFile.isDirectory()) {
                logger.debug("目录已存在: {}", dirFile.getAbsolutePath());
                return true;
            }

            logger.error("已存在同名文件: {}", dirFile.getAbsolutePath());
            return false;
        }

        if (!tryMake) {
            logger.info("目录不存在 {}", dirFile.getAbsolutePath());
            return false;
        }

        try {
            Files.createDirectories(dirFile.toPath());
            logger.info("创建目录: {}", dirFile.getAbsolutePath());
            return true;
        } catch (FileAlreadyExistsException e) {
            logger.warn("尝试创建目录但已存在: {}", dirFile.getAbsolutePath());
            return true;
        } catch (IOException e) {
            logger.error("error {} ", dirFile.getAbsolutePath(), e);
            return false;
        }
    }

    /**
     * 根据文件路径生成Writer对象
     *
     * @param filePath 文件路径
     * @return
     */
    public static BufferedWriter genBufferedWriter(String filePath) throws FileNotFoundException {
        return genBufferedWriter(filePath, false);
    }

    /**
     * 根据文件路径生成Writer对象
     *
     * @param filePath 文件路径
     * @param append   是否在已有文件后追加
     * @return
     */
    public static BufferedWriter genBufferedWriter(String filePath, boolean append) throws FileNotFoundException {
        return new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filePath, append), StandardCharsets.UTF_8));
    }

    public static File findFile(String filePath) {
        // 尝试通过文件路径获取文件
        File file = new File(filePath);
        if (file.exists()) {
            logger.info("通过文件路径获取文件 {}", filePath);
            return file;
        }

        // 尝试从classpath中获取文件，路径以/开头
        URL url = JavaCGFileUtil.class.getResource("/" + filePath);
        if (url != null && "file".equals(url.getProtocol())) {
            /*
                当URL中的protocol为"file"时，说明对应的资源为独立文件的形式
                若为"jar"则说明对应的资源是jar包中的文件，不能通过以下方式处理
             */
            logger.info("从classpath中获取文件 {}", url);
            try {
                return new File(url.toURI());
            } catch (Exception e) {
                logger.error("error ", e);
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
            logger.error("未找到文件 {}", filePath);
            throw new JavaCGRuntimeException("未找到文件 " + filePath);
        }

        logger.info("从jar包中获取文件 {}", JavaCGFileUtil.class.getResource("/" + filePath));
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
            logger.error("处理文件出现异常 {} ", filePath, e);
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
            logger.error("处理文件出现异常 {} ", filePath, e);
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
