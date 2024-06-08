package com.adrninistrator.javacg.conf;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description: 配置参数管理类
 */
public class JavaCGConfManager {
    private static final Logger logger = LoggerFactory.getLogger(JavaCGConfManager.class);

    public static JavaCGConfInfo getConfInfo(JavaCGConfigureWrapper javaCGConfigureWrapper) {
        JavaCGConfInfo confInfo = new JavaCGConfInfo();
        // 获取config.properties中的配置参数，路径需要使用"/"
        String configFilePath = getInputRootPath() + JavaCGConstants.DIR_CONFIG + "/" + JavaCGConstants.FILE_CONFIG;
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(JavaCGFileUtil.getFileInputStream(configFilePath))) {
            Properties properties = new Properties();
            properties.load(br);

            confInfo.setParseMethodCallTypeValue(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, true)));
            confInfo.setFirstParseInitMethodType(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, true)));
            confInfo.setAnalyseFieldRelationship(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, true)));
            confInfo.setContinueWhenError(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, true)));
            confInfo.setLogMethodSpendTime(Boolean.parseBoolean(javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_LOG_METHOD_SPEND_TIME, true)));

            String outputRootPath = javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, true);
            confInfo.setOutputRootPath(outputRootPath);

            String outputFileExt = javaCGConfigureWrapper.getConfig(properties, JavaCGConfigKeyEnum.CKE_OUTPUT_FILE_EXT, true);
            if (StringUtils.isBlank(outputFileExt)) {
                // 默认使用.txt作为输出文件后缀名
                confInfo.setOutputFileExt(JavaCGConstants.EXT_TXT);
            } else {
                confInfo.setOutputFileExt(outputFileExt);
            }
        } catch (Exception e) {
            logger.error("获取配置参数出现异常 ", e);
            return null;
        }

        // 获取jar_dir.properties中的配置参数
        List<String> jarDirList = javaCGConfigureWrapper.getOtherConfigList(JavaCGOtherConfigFileUseListEnum.OCFULE_JAR_DIR, true);
        confInfo.setJarDirList(jarDirList);

        // 获取packages.properties中的配置参数
        Set<String> needHandlePackageSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_PACKAGES, true);
        confInfo.setNeedHandlePackageSet(needHandlePackageSet);

        // 获取jar_dir_merge_file_type.properties中的配置参数
        Set<String> tmpJarDirMergeFileTypeSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_JAR_DIR_MERGE_FILE_TYPE, true);
        Set<String> jarDirMergeFileTypeSet = new HashSet<>(tmpJarDirMergeFileTypeSet.size());
        for (String tmpJarDirMergeFileType : tmpJarDirMergeFileTypeSet) {
            // 文件类型使用小写
            jarDirMergeFileTypeSet.add(tmpJarDirMergeFileType.toLowerCase());
        }
        confInfo.setJarDirMergeFileTypeSet(jarDirMergeFileTypeSet);

        // 获取ignore_jar_file_keyword.properties中的配置参数
        Set<String> ignoreJarFileKeywordSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_IGNORE_JAR_FILE_KEYWORD, true);
        confInfo.setIgnoreJarFileKeywordSet(ignoreJarFileKeywordSet);

        // 获取ignore_jar_file_name.properties中的配置参数
        Set<String> ignoreJarFileNameSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_IGNORE_JAR_FILE_NAME, true);
        confInfo.setIgnoreJarFileNameSet(ignoreJarFileNameSet);

        // 处理fr_eq_conversion_method.properties中的配置参数
        if (!handleFrEqConversionMethod(javaCGConfigureWrapper, confInfo)) {
            return null;
        }
        return confInfo;
    }

    // 处理fr_eq_conversion_method.properties中的配置参数
    private static boolean handleFrEqConversionMethod(JavaCGConfigureWrapper javaCGConfigureWrapper, JavaCGConfInfo confInfo) {
        Map<String, Map<String, Integer>> frEqConversionMethodMap = new HashMap<>();
        confInfo.setFrEqConversionMethodMap(frEqConversionMethodMap);
        Set<String> frEqConversionMethodSet = javaCGConfigureWrapper.getOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, true);
        if (JavaCGUtil.isCollectionEmpty(frEqConversionMethodSet)) {
            return true;
        }
        for (String frEqConversionMethod : frEqConversionMethodSet) {
            String[] data1 = StringUtils.splitPreserveAllTokens(frEqConversionMethod, JavaCGConstants.FLAG_EQUAL);
            if (data1.length != 2) {
                logger.error("配置文件内容不是合法的properties参数 {} {}", JavaCGOtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getFileName(), frEqConversionMethod);
                return false;
            }
            String classAndMethod = data1[0];
            int argObjSeq = Integer.parseInt(data1[1]);
            if (argObjSeq < 0) {
                logger.error("配置文件被调用对象（使用0表示）或方法参数（从1开始）序号非法 {} {}", JavaCGOtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getFileName(), frEqConversionMethod);
                return false;
            }
            String[] data2 = StringUtils.splitPreserveAllTokens(classAndMethod, JavaCGConstants.FLAG_COLON);
            if (data2.length != 2) {
                logger.error("配置文件内容不是合法的类名与方法名 {} {}", JavaCGOtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getFileName(), frEqConversionMethod);
                return false;
            }
            String className = data2[0];
            String methodName = data2[1];
            Map<String, Integer> methodMap = frEqConversionMethodMap.computeIfAbsent(className, k -> new HashMap<>());
            methodMap.put(methodName, argObjSeq);
        }
        return true;
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
