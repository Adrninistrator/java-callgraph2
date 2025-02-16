package com.adrninistrator.javacg2.targz;

import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.tools.tar.TarEntry;
import org.apache.tools.tar.TarInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;

/**
 * @author adrninistrator
 * @date 2024/1/18
 * @description: 用于解压.tar.gz中的war包、jar包及配置文件
 */
public class TarGzUnpacker {
    private static final Logger logger = LoggerFactory.getLogger(TarGzUnpacker.class);

    // 需要解压的tar.gz文件路径
    protected final String tarGzFilePath;

    // 保存解压出文件的目录路径
    private final String unpackDstDirPath;

    private String tarGzFileName;

    /**
     * @param tarGzFilePath    需要解压的tar.gz文件路径
     * @param unpackDstDirPath 保存解压出文件的目录路径，需要保证执行前该目录不存在或为空
     */
    public TarGzUnpacker(String tarGzFilePath,
                         String unpackDstDirPath) {
        this.tarGzFilePath = tarGzFilePath;
        this.unpackDstDirPath = unpackDstDirPath;
    }

    /**
     * 执行解压的方法
     *
     * @return true: 成功 false: 失败
     */
    public boolean unpack() {
        logger.info("开始处理文件 {}", tarGzFilePath);
        File tarGzFile = new File(tarGzFilePath);
        if (!tarGzFile.exists()) {
            logger.error("文件不存在 {}", tarGzFilePath);
            return false;
        }
        tarGzFileName = tarGzFile.getName();

        if (!JavaCG2FileUtil.isDirectoryExists(unpackDstDirPath, true)) {
            logger.error("输出目录不存在且无法创建 {} {}", tarGzFileName, unpackDstDirPath);
            return false;
        }
        File[] files = new File(unpackDstDirPath).listFiles();
        if (!ArrayUtils.isEmpty(files)) {
            logger.error("保存解压后文件的目录非空，请先清空该目录 {}", unpackDstDirPath);
            return false;
        }

        try (TarInputStream tarInput = new TarInputStream(new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(tarGzFile))))) {
            TarEntry tarEntry;
            while ((tarEntry = tarInput.getNextEntry()) != null) {
                if (!tarEntry.isFile()) {
                    continue;
                }

                handleFileInTar(tarInput, tarEntry);
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} ", tarGzFileName, e);
            return false;
        }
    }

    protected boolean handleFileInTar(TarInputStream inputStream, TarEntry tarEntry) {
        String tarEntryName = tarEntry.getName();
        // 生成的文件目录保持原有形式
        String filePath = unpackDstDirPath + File.separator + tarEntryName;
        logger.info("保存文件 {} {} {}", tarGzFileName, tarEntryName, filePath);
        File file = new File(filePath);
        if (!JavaCG2FileUtil.isDirectoryExists(file.getParent(), true)) {
            logger.error("创建文件所在目录失败 {}", file.getParent());
            return false;
        }
        return JavaCG2FileUtil.saveInputToFileNoClose(inputStream, file);
    }
}
