package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Collection;
import java.util.Date;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/22
 * @description:
 */

public class JavaCGUtil {

    /**
     * 判断类名是否为匿名内部类
     *
     * @param className
     * @return
     */
    public static boolean isInnerAnonymousClass(String className) {
        String tail = StringUtils.substringAfterLast(className, "$");
        return isNumStr(tail);
    }

    /**
     * 判断字符串是否为数字
     *
     * @param str
     * @return
     */
    public static boolean isNumStr(String str) {
        if (StringUtils.isBlank(str)) {
            return false;
        }

        char[] charArray = str.toCharArray();
        for (char ch : charArray) {
            if (ch < '0' || ch > '9') {
                return false;
            }
        }
        return true;
    }

    /**
     * 将不可重复读的InputStream缓存为可以重复读取的ByteArrayInputStream
     *
     * @param inputStream
     * @return
     */
    public static InputStream cacheInputStream(InputStream inputStream) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            byte[] data = new byte[4096];
            int size;
            while ((size = inputStream.read(data)) != -1) {
                baos.write(data, 0, size);
            }

            return new ByteArrayInputStream(baos.toByteArray());
        } catch (Exception e) {
            System.err.println("出现异常 " + e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    /**
     * 判断是否为Object类
     *
     * @param className
     * @return
     */
    public static boolean isObjectClass(String className) {
        return JavaCGCommonNameConstants.CLASS_NAME_OBJECT.equals(className);
    }

    /**
     * 判断指定类是否为JDK中的类
     *
     * @param className
     * @return
     */
    public static boolean isClassInJdk(String className) {
        return StringUtils.startsWith(className, "java.");
    }

    /**
     * 判断是否为<init>方法
     *
     * @param methodName
     * @return
     */
    public static boolean isInitMethod(String methodName) {
        return JavaCGCommonNameConstants.METHOD_NAME_INIT.equals(methodName);
    }

    /**
     * 判断字符串是否以指定的字符串数组结尾，忽略大小写
     *
     * @param data
     * @param array
     * @return
     */
    public static boolean isStringEndWithArrayIgnoreCase(String data, String[] array) {
        if (data == null || array == null || array.length == 0) {
            return false;
        }

        for (String arrayStr : array) {
            if (StringUtils.endsWithIgnoreCase(data, arrayStr)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 从完整类名中获取简单类名（去掉包名）
     *
     * @param className 完整类名
     * @return
     */
    public static String getSimpleClassNameFromFull(String className) {
        int indexLastDot = className.lastIndexOf(JavaCGConstants.FLAG_DOT);
        if (indexLastDot == -1) {
            return className;
        }
        return className.substring(indexLastDot + 1);
    }

    /**
     * 获取简单类名首字母小写后的结果
     *
     * @param simpleClassName 简单类名
     * @return
     */
    public static String getFirstLetterLowerClassName(String simpleClassName) {
        if (simpleClassName == null) {
            return null;
        }

        if (simpleClassName.isEmpty()) {
            return "";
        }

        String firstLetterLower = simpleClassName.substring(0, 1).toLowerCase();
        if (simpleClassName.length() == 1) {
            return firstLetterLower;
        }

        return firstLetterLower + simpleClassName.substring(1);
    }

    /**
     * 获取当前时间
     *
     * @return
     */
    public static String currentTime() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd-HHmmss.SSS");
        return sdf.format(new Date());
    }

    /**
     * 判断是否需要跳过当前类的处理
     *
     * @param className
     * @param needHandlePackageSet
     * @return false: 不跳过 true: 跳过
     */
    public static boolean checkSkipClass(String className, Set<String> needHandlePackageSet) {
        if (isCollectionEmpty(needHandlePackageSet)) {
            return false;
        }
        for (String needHandlePackage : needHandlePackageSet) {
            if (StringUtils.startsWith(className, needHandlePackage)) {
                return false;
            }
        }
        return true;
    }

    /**
     * 为文件路径增加分隔符
     *
     * @param filePath
     * @return
     */
    public static String addSeparator4FilePath(String filePath) {
        if (StringUtils.endsWithAny(filePath, "/", "\\")) {
            // 文件路径以分隔符结尾，则直接使用
            return filePath;
        }

        // 文件路径没有以分隔符结尾，则在后面增加分隔符
        return filePath + File.separator;
    }

    /**
     * 获取JVM参数中指定的目录路径
     *
     * @param jvmOptionKey
     * @return
     */
    public static String getDirPathInJvmOptions(String jvmOptionKey) {
        String dirPath = System.getProperty(jvmOptionKey);
        if (dirPath == null) {
            return "";
        }

        return addSeparator4FilePath(dirPath);
    }

    /**
     * base64编码
     *
     * @param data
     * @return
     */
    public static String base64Encode(String data) {
        return Base64.getEncoder().encodeToString(data.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * base64解码
     *
     * @param data
     * @return
     */
    public static String base64Decode(String data) {
        return new String(Base64.getDecoder().decode(data), StandardCharsets.UTF_8);
    }

    /**
     * 判断集合是否为空
     *
     * @param collection
     * @param <T>
     * @return
     */
    public static <T> boolean isCollectionEmpty(Collection<T> collection) {
        return collection == null || collection.isEmpty();
    }

    private JavaCGUtil() {
        throw new IllegalStateException("illegal");
    }
}