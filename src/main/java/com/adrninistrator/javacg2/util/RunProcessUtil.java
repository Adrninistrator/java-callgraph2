package com.adrninistrator.javacg2.util;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2025/7/9
 * @description: 启动进程工具
 */
public class RunProcessUtil {

    private static final Logger logger = LoggerFactory.getLogger(RunProcessUtil.class);

    private static final ThreadFactory threadFactoryStdout = genThreadFactory("wait_stdout");
    private static final ThreadFactory threadFactoryStderr = genThreadFactory("wait_stderr");

    private static ThreadFactory genThreadFactory(String threadNamePrefix) {
        return new ThreadFactory() {
            private final AtomicInteger seq = new AtomicInteger(0);

            @Override
            public Thread newThread(Runnable r) {
                Thread thread = new Thread(r);
                thread.setDaemon(false);
                thread.setName(threadNamePrefix + "-" + seq.addAndGet(1));
                return thread;
            }
        };
    }

    /**
     * 启动进程
     *
     * @param args
     * @return
     */
    public static boolean runProcess(String[] args) {
        logger.info("启动进程时使用的参数 {}", StringUtils.join(args, " "));
        ProcessBuilder processBuilder = new ProcessBuilder(args);
        try {
            Process childProcess = processBuilder.start();

            // 等待子进程结束
            int exitCode = RunProcessUtil.waitProcess(childProcess);
            if (exitCode != 0) {
                logger.error("子进程执行失败，返回码 {}", exitCode);
                return false;
            }
            logger.info("子进程执行返回码 {}", exitCode);
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    private static int waitProcess(Process process) {
        Thread threadStdout = threadFactoryStdout.newThread(() -> {
            /* MDC 内容复制 */
            try (BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = in.readLine()) != null) {
                    logger.info("stdout {}", line);
                }
            } catch (Exception e) {
                logger.error("error ", e);
            } finally {
                /* MDC 内容清理 */
            }
        });

        Thread threadStderr = threadFactoryStderr.newThread(() -> {
            /* MDC 内容复制 */
            try (BufferedReader in = new BufferedReader(new InputStreamReader(process.getErrorStream()))) {
                String line;
                while ((line = in.readLine()) != null) {
                    logger.info("stderr {}", line);
                }
            } catch (Exception e) {
                logger.error("error ", e);
            } finally {
                /* MDC 内容清理 */
            }
        });

        threadStdout.start();
        threadStderr.start();

        try {
            threadStdout.join();
            threadStderr.join();
            return process.waitFor();
        } catch (InterruptedException e) {
            logger.error("error ", e);
            Thread.currentThread().interrupt();
            return -1;
        }
    }

    /**
     * 处理启动进程的参数，假如包含空格，则使用双引号包含
     *
     * @param arg
     * @return
     */
    public static String handleProcessArg(String arg) {
        if (arg.contains(" ")) {
            return JavaCG2Util.wrapWithQuotes(arg);
        }
        return arg;
    }
}
