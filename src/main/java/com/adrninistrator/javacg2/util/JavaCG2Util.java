package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
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
        if (data == null || array == null) {
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

        String newDirPath = JavaCG2FileUtil.addSeparator4FilePath(dirPath);
        logger.debug("获取到JVM参数 {} {} 在路径结尾增加分隔符为 {}", jvmOptionKey, dirPath, newDirPath);
        return newDirPath;
    }

    /**
     * 判断字符串是否需要base64编码
     *
     * @param data
     * @return
     */
    public static boolean checkNeedBase64(String data) {
        return StringUtils.containsAny(data, "\r", "\n", "\t");
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
     * 从字符串str中查找最后的标记字符串flag之后的字符串，若源字符串中没有标记字符串时，返回整个源字符串
     *
     * @param str  源字符串
     * @param flag 标记字符串
     * @return
     */
    public static String getSubStringAfterLast(String str, String flag) {
        // 不使用StringUtils.substringAfterLast，因为当源字符串没有标记字符串时结果为空
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

    /**
     * 将毫秒数转换为秒
     *
     * @param spendTime
     * @return
     */
    public static double getSecondsFromMilli(long spendTime) {
        return spendTime / 1000.0D;
    }

    /**
     * 获得经过的时间秒数
     *
     * @param startTime
     * @return
     */
    public static double getSpendSeconds(long startTime) {
        return getSecondsFromMilli(System.currentTimeMillis() - startTime);
    }

    /**
     * 将源列表中的元素添加到目标列表中
     * 忽略空的元素，忽略重复的元素
     *
     * @param srcList
     * @param destList
     */
    public static void addList2List(List<String> srcList, List<String> destList) {
        if (srcList == null || destList == null) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }
        for (String src : srcList) {
            if (StringUtils.isBlank(src) || destList.contains(src)) {
                continue;
            }
            destList.add(src);
        }
    }

    /**
     * 获取配置文件的根目录
     *
     * @return
     */
    public static String getInputRootPath() {
        return getDirPathInJvmOptions(JavaCG2Constants.JVM_PROP_KEY_INPUT_ROOT_PATH);
    }

    /**
     * 获取对象用于打印的字符串值
     *
     * @param value
     * @return
     */
    public static String getObjectPrintValue(Object value) {
        if (value == null) {
            return "";
        } else if (value instanceof String) {
            return (String) value;
        }
        return value.toString();
    }

    /**
     * 将Map所有的key、value转换为字符串
     *
     * @param map
     * @return
     */
    public static String getMapValueStr(Map<String, Object> map) {
        StringBuilder stringBuilder = new StringBuilder("{");
        List<String> keyList = new ArrayList<>(map.keySet());
        Collections.sort(keyList);
        boolean first = true;
        for (String key : keyList) {
            Object value = map.get(key);
            if (!first) {
                stringBuilder.append(", ");
            }
            stringBuilder.append(key).append("=");
            if (value instanceof Set) {
                String valueString = StringUtils.join((Set<?>) value, ", ");
                stringBuilder.append("(").append(valueString).append(")");
            } else {
                stringBuilder.append(value);
            }
            first = false;
        }
        return stringBuilder.append("}").toString();
    }

    /**
     * 等待指定时间
     *
     * @param time
     */
    public static void sleep(long time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
            logger.error("error ", e);
            Thread.currentThread().interrupt();
        }
    }

    /**
     * 检查是否为Windows操作系统
     *
     * @return
     */
    public static boolean checkWindowsOs() {
        String osName = System.getProperty("os.name");
        return StringUtils.startsWithIgnoreCase(osName, "Windows");
    }

    /**
     * 检查是否为示例表达式配置文件
     *
     * @param elConfig
     * @return
     */
    public static boolean checkElExample(ElConfigInterface elConfig) {
        return elConfig.getKey().endsWith(JavaCG2Constants.EXT_MD);
    }

    /**
     * 在字符串前后增加双引号
     *
     * @param data
     * @return
     */
    public static String wrapWithQuotes(String data) {
        return "\"" + data + "\"";
    }

    private JavaCG2Util() {
        throw new IllegalStateException("illegal");
    }
}