package com.adrninistrator.javacg2.el.manager;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.el.checker.ElChecker;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.handler.ElHandler;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.thread.ThreadFactory4TPE;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 表达式管理类基类
 */
public abstract class ElManager implements Closeable {

    private static final Logger logger = LoggerFactory.getLogger(ElManager.class);
    /*
            保存表达式处理类的Map
            key     配置文件名称
            value   表达式处理类
         */
    private final Map<String, ElHandler> elHandlerMap = new HashMap<>();

    // 用于写表达式忽略数据的文件输出流
    private final Writer elIgnoreDataWriter;

    // 用于写表达式忽略数据文件的线程池，在java-all-call-graph中可能在多线程中被调用
    private final ThreadPoolExecutor writeFileTPE;

    private boolean closed = false;

    protected ElManager(BaseConfigureWrapper configureWrapper, ElConfigInterface[] elConfigInterfaces, String outputDirPath) {
        String elIgnoreDataFilePath = outputDirPath + File.separator + JavaCG2Constants.EL_IGNORE_DATA_FILE_NAME;
        logger.info("保存表达式忽略数据的文件路径 {}", elIgnoreDataFilePath);
        try {
            elIgnoreDataWriter = JavaCG2FileUtil.genBufferedWriter(elIgnoreDataFilePath);
        } catch (FileNotFoundException e) {
            logger.error("出现异常 ", e);
            throw new JavaCG2RuntimeException();
        }

        ThreadFactory4TPE threadFactory4TPE = new ThreadFactory4TPE(JavaCG2Constants.THREAD_NAME_PREFIX_EL_WRITE_IGNORE_DATA);
        writeFileTPE = new ThreadPoolExecutor(1, 1, 10L, TimeUnit.SECONDS, new LinkedBlockingQueue<>(), threadFactory4TPE);

        // 在JVM关闭时检查是否有执行关闭操作
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            if (!closed) {
                logger.error("未执行关闭操作");
                close();
            }
        }));

        // 生成ElHandler对象
        for (ElConfigInterface elConfig : elConfigInterfaces) {
            Class<? extends ElChecker> elCheckerClass = elConfig.getElCheckClass();
            if (elCheckerClass == null) {
                continue;
            }
            String fileName = elConfig.getKey();
            // 匹配配置文件中的表达式文本
            String elText = configureWrapper.getElConfigText(elConfig);
            ElHandler elHandler = new ElHandler(elText, elConfig, elIgnoreDataWriter, writeFileTPE);
            elHandlerMap.put(fileName, elHandler);

            // 检查表达式
            try {
                ElChecker elChecker = elCheckerClass.newInstance();
                elChecker.check(this, elConfig);
            } catch (Exception e) {
                logger.error("创建对象实例失败 {}", elCheckerClass.getName());
                throw new JavaCG2RuntimeException("创建对象实例失败");
            }
        }
        // 选择是否为调试模式
        boolean debugMode = chooseDebugMode(configureWrapper);
        if (debugMode) {
            for (ElHandler elHandler : elHandlerMap.values()) {
                elHandler.setDebugMode(true);
            }
        }
    }

    /**
     * 选择是否为调试模式
     *
     * @param configureWrapper
     * @return
     */
    protected abstract boolean chooseDebugMode(BaseConfigureWrapper configureWrapper);

    public ElHandler getElHandlerMap(ElConfigInterface elConfig) {
        ElHandler elHandler = elHandlerMap.get(elConfig.getKey());
        if (elHandler == null) {
            throw new JavaCG2RuntimeException("未找到对应的表达式处理类 " + elConfig.getKey());
        }
        return elHandler;
    }

    @Override
    public void close() {
        closed = true;
        if (writeFileTPE != null) {
            writeFileTPE.shutdown();
            try {
                writeFileTPE.awaitTermination(10, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                logger.error("出现异常 ", e);
                throw new JavaCG2RuntimeException();
            }
        }
        if (elIgnoreDataWriter != null) {
            IOUtils.closeQuietly(elIgnoreDataWriter);
        }
    }
}
