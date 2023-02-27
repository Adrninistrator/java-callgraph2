package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;

import java.io.File;
import java.io.Writer;
import java.lang.management.ManagementFactory;

/**
 * @author adrninistrator
 * @date 2022/10/18
 * @description:
 */
public class JavaCGLogUtil {

    // 调试日志打印开关
    private static boolean DEBUG_PRINT_FLAG = false;

    // 调试日志打印到文件开关
    private static boolean DEBUG_PRINT_IN_FILE = false;

    public static Writer LOG_WRITER = null;

    // 当前是否处于调试状态开关
    private static boolean DEBUG_FLAG;

    public static boolean isDebugPrintFlag() {
        return DEBUG_PRINT_FLAG;
    }

    public static void setDebugPrintFlag(boolean debugPrintFlag) {
        if (DEBUG_PRINT_FLAG && !debugPrintFlag) {
            System.err.println("DEBUG_PRINT_FLAG已打开，不能再关闭");
            return;
        }

        DEBUG_PRINT_FLAG = debugPrintFlag;
        if (DEBUG_PRINT_FLAG) {
            for (String arg : ManagementFactory.getRuntimeMXBean().getInputArguments()) {
                if (arg.startsWith("-Xrunjdwp") || arg.startsWith("-agentlib:jdwp")) {
                    DEBUG_FLAG = true;
                    break;
                }
            }
        }
    }

    public static boolean isDebugPrintInFile() {
        return DEBUG_PRINT_IN_FILE;
    }

    public static void setDebugPrintInFile(boolean debugPrintInFile) {
        if (DEBUG_PRINT_IN_FILE && !debugPrintInFile) {
            System.err.println("DEBUG_PRINT_FLAG已打开，不能再关闭");
            return;
        }

        DEBUG_PRINT_IN_FILE = debugPrintInFile;
        if (DEBUG_PRINT_IN_FILE) {
            JavaCGFileUtil.isDirectoryExists(JavaCGConstants.DIR_LOG, true);

            String logPath = JavaCGConstants.DIR_LOG + File.separator + JavaCGConstants.FILE_PREFIX_LOG + JavaCGUtil.currentTime() + JavaCGConstants.EXT_LOG;
            System.out.println("将日志写入文件中 " + logPath);
            try {
                LOG_WRITER = JavaCGFileUtil.genBufferedWriter(logPath);
            } catch (Exception e) {
                e.printStackTrace();
            }

            Runtime.getRuntime().addShutdownHook(new Thread(() -> beforeExit()));
        }
    }

    /**
     * 判断当前是否可能在调试
     *
     * @return
     */
    public static boolean checkDebugFlag() {
        return DEBUG_FLAG;
    }

    /**
     * 打印调试用日志
     *
     * @param data
     */
    public static void debugPrint(String data) {
        if (LOG_WRITER != null) {
            try {
                LOG_WRITER.write(data + JavaCGConstants.NEW_LINE);
            } catch (Exception e) {
                e.printStackTrace();
            }
            return;
        }

        System.out.println(data);
    }

    public static void beforeExit() {
        System.out.println("JavaCGLogUtil.beforeExit");

        if (LOG_WRITER != null) {
            try {
                LOG_WRITER.flush();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private JavaCGLogUtil() {
        throw new IllegalStateException("illegal");
    }
}
