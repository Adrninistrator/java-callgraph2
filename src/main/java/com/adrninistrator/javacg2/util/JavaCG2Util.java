package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/22
 * @description:
 */

public class JavaCG2Util {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2Util.class);

    private static volatile Boolean DEBUG_MODE;

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
            byte[] data = new byte[8192];
            int size;
            while ((size = inputStream.read(data)) != -1) {
                baos.write(data, 0, size);
            }

            return new ByteArrayInputStream(baos.toByteArray());
        } catch (Exception e) {
            logger.error("出现异常 ", e);
            return null;
        }
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
     * 获取当前时间
     *
     * @return
     */
    public static String currentTime() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd-HHmmss.SSS");
        return sdf.format(new Date());
    }

    /**
     * 判断是否需要跳过当前类的处理，白名单方式
     *
     * @param className
     * @param needHandlePackageSet
     * @return true: 跳过 false: 不跳过
     */
    public static boolean checkSkipClassWhiteList(String className, Set<String> needHandlePackageSet) {
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
     * 判断是否需要跳过当前类的处理，黑名单方式
     *
     * @param className
     * @param ignoreClassNameSet
     * @return true: 跳过 false: 不跳过
     */
    public static boolean checkSkipClassBlackList(String className, Set<String> ignoreClassNameSet) {
        if (isCollectionEmpty(ignoreClassNameSet)) {
            return false;
        }
        return ignoreClassNameSet.contains(className);
    }

    /**
     * 判断合并jar包、目录时，当前处理的文件的类型是否需要处理
     *
     * @param fileName
     * @param jarDirMergeFileTypeSet
     * @return true: 需要处理 false: 不需要处理
     */
    public static boolean checkMergeFileType(String fileName, Set<String> jarDirMergeFileTypeSet) {
        String fileNameLower = fileName.toLowerCase();
        for (String jarDirMergeFileType : jarDirMergeFileTypeSet) {
            if (fileNameLower.endsWith(jarDirMergeFileType)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 为文件路径增加分隔符
     *
     * @param filePath
     * @return
     */
    public static String addSeparator4FilePath(String filePath) {
        if (StringUtils.endsWithAny(filePath, JavaCG2Constants.FLAG_SLASH, "\\")) {
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

    /**
     * 判断Map是否为空
     *
     * @param map
     * @return
     */
    public static boolean isMapEmpty(Map map) {
        return (map == null || map.isEmpty());
    }

    /**
     * 根据不定长数组生成HashSet（可修改）
     *
     * @param array
     * @param <T>
     * @return
     */
    @SafeVarargs
    public static <T> Set<T> genSetFromArray(T... array) {
        Set<T> set = new HashSet<>();
        if (!ArrayUtils.isEmpty(array)) {
            set.addAll(Arrays.asList(array));
        }
        return set;
    }

    /**
     * 根据不定长数组生成List（可修改）
     *
     * @param array
     * @param <T>
     * @return
     */
    @SafeVarargs
    public static <T> List<T> genListFromArray(T... array) {
        List<T> list = new ArrayList<>();
        if (!ArrayUtils.isEmpty(array)) {
            list.addAll(Arrays.asList(array));
        }
        return list;
    }

    /**
     * 从字符串str中查找最后的标记字符串flag之后的字符串
     *
     * @param str  源字符串
     * @param flag 标记字符串
     * @return
     */
    public static String getSubStringAfterLast(String str, String flag) {
        // 不使用StringUtils.substringAfterLast，因为当没有指定的标记字符串时结果为空
        int lastIndex = StringUtils.lastIndexOf(str, flag);
        if (lastIndex == -1) {
            return str;
        }
        return str.substring(lastIndex + flag.length());
    }

    /**
     * 获得Integer对应的字符串，若为空则返回空字符串
     *
     * @param i
     * @return
     */
    public static String genStringFromInteger(Integer i) {
        if (i == null) {
            return "";
        }
        return String.valueOf(i);
    }

    /**
     * 获得字符串对应的Integer，若为空字符串则返回空
     *
     * @param s
     * @return
     */
    public static Integer genIntegerFromString(String s) {
        if (StringUtils.isBlank(s)) {
            return null;
        }
        return Integer.valueOf(s);
    }

    /**
     * 判断当前是否是调试模式
     *
     * @return
     */
    public static boolean checkInDebugMode() {
        if (DEBUG_MODE != null) {
            return DEBUG_MODE;
        }
        for (String arg : ManagementFactory.getRuntimeMXBean().getInputArguments()) {
            if (arg.startsWith("-Xrunjdwp") || arg.startsWith("-agentlib:jdwp")) {
                logger.info("当前是调试模式");
                DEBUG_MODE = Boolean.TRUE;
                return true;
            }
        }
        logger.info("当前不是调试模式");
        DEBUG_MODE = Boolean.FALSE;
        return false;
    }

    private JavaCG2Util() {
        throw new IllegalStateException("illegal");
    }
}