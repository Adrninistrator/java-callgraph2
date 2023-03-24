package com.adrninistrator.javacg.conf;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;

import java.io.BufferedReader;
import java.util.List;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description:
 */
public class JavaCGConfManager {

    public static JavaCGConfInfo getConfInfo(JavaCGConfigureWrapper javaCGConfigureWrapper) {
        if (javaCGConfigureWrapper == null) {
            return null;
        }

        JavaCGConfInfo confInfo = new JavaCGConfInfo();

        // 获取config.properties中的配置参数，路径需要使用"/"
        String configFilePath = getInputRootPath() + JavaCGConstants.DIR_CONFIG + "/" + JavaCGConstants.FILE_CONFIG;
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(JavaCGFileUtil.getFileInputStream(configFilePath))) {
            Properties properties = new Properties();
            properties.load(br);

            confInfo.setParseMethodCallTypeValue(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, true)));
            confInfo.setFirstParseInitMethodType(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, true)));
            confInfo.setContinueWhenError(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, true)));

            String debugPrintStr = javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_DEBUG_PRINT, true);
            if (JavaCGConstants.PROPERTY_VALUE_DEBUG_PRINT_IN_FILE.equals(debugPrintStr)) {
                confInfo.setDebugPrint(true);
                confInfo.setDebugPrintInFile(true);
            } else if (Boolean.parseBoolean(debugPrintStr)) {
                confInfo.setDebugPrint(true);
                confInfo.setDebugPrintInFile(false);
            }

            String outputFileExt = javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_OUTPUT_FILE_EXT, true);
            if (StringUtils.isBlank(outputFileExt)) {
                // 默认使用.txt作为输出文件后缀名
                confInfo.setOutputFileExt(JavaCGConstants.EXT_TXT);
            } else {
                confInfo.setOutputFileExt(outputFileExt);
            }
        } catch (Exception e) {
            System.err.println("获取配置参数出现异常");
            e.printStackTrace();
        }

        // 获取jar_dir.properties中的配置参数
        List<String> jarDirList = javaCGConfigureWrapper.getOtherConfigList(JavaCGOtherConfigFileUseListEnum.OCFULE_JAR_DIR, true);
        confInfo.setJarDirList(jarDirList);

        // 获取packages.properties中的配置参数
        Set<String> needHandlePackageSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_PACKAGES, true);
        confInfo.setNeedHandlePackageSet(needHandlePackageSet);

        return confInfo;
    }

    /**
     * 获取配置文件根目录
     *
     * @return
     */
    public static String getInputRootPath() {
        return JavaCGUtil.getDirPathInJvmOptions(JavaCGConstants.PROPERTY_INPUT_ROOT_PATH);
    }

    private JavaCGConfManager() {
        throw new IllegalStateException("illegal");
    }
}
