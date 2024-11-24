package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
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
public class JavaCG2ConfManager {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ConfManager.class);

    public static JavaCG2ConfInfo getConfInfo(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper) {
        String configFileName = JavaCG2Constants.FILE_PATH_CONFIG;
        JavaCG2ConfInfo confInfo = new JavaCG2ConfInfo();
        // 获取config.properties中的配置参数，路径需要使用JavaCG2Constants.FLAG_SLASH
        String configFilePath = getInputRootPath() + configFileName;
        try (BufferedReader br = JavaCG2FileUtil.genBufferedReader(JavaCG2FileUtil.getFileInputStream(configFilePath))) {
            Properties properties = new Properties();
            properties.load(br);

            boolean parseMethodCallTypeValue = Boolean.parseBoolean(javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, true));
            boolean firstParseInitMethodType = Boolean.parseBoolean(javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, true));
            boolean analyseFieldRelationship = Boolean.parseBoolean(javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, true));

            if (!parseMethodCallTypeValue) {
                if (firstParseInitMethodType) {
                    logger.error("配置文件 {} 中的 {} 参数值为true时， {} 参数值也需要为true。假如不需要使用，请都设置为false", configFileName, JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE.getKey(),
                            JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey());
                    return null;
                }
                if (analyseFieldRelationship) {
                    logger.error("配置文件 {} 中的 {} 参数值为true时， {} 参数值也需要为true。假如不需要使用，请都设置为false", configFileName, JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP.getKey(),
                            JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey());
                    return null;
                }
            }

            confInfo.setParseMethodCallTypeValue(parseMethodCallTypeValue);
            confInfo.setFirstParseInitMethodType(firstParseInitMethodType);
            confInfo.setAnalyseFieldRelationship(analyseFieldRelationship);
            confInfo.setContinueWhenError(Boolean.parseBoolean(javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, true)));
            confInfo.setLogMethodSpendTime(Boolean.parseBoolean(javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_LOG_METHOD_SPEND_TIME, true)));

            String outputRootPath = javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, true);
            confInfo.setOutputRootPath(outputRootPath);

            String outputFileExt = javaCG2ConfigureWrapper.getConfig(properties, JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT, true);
            if (StringUtils.isBlank(outputFileExt)) {
                // 默认使用.txt作为输出文件后缀名
                confInfo.setOutputFileExt(JavaCG2Constants.EXT_TXT);
            } else {
                confInfo.setOutputFileExt(outputFileExt);
            }
        } catch (Exception e) {
            logger.error("获取配置参数出现异常 ", e);
            return null;
        }

        // 获取jar_dir.properties中的配置参数
        List<String> jarDirList = javaCG2ConfigureWrapper.getOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, true);
        confInfo.setJarDirList(jarDirList);

        // 获取packages.properties中的配置参数
        Set<String> needHandlePackageSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_PACKAGES, true);
        confInfo.setNeedHandlePackageSet(needHandlePackageSet);

        // 获取jar_dir_merge_file_type.properties中的配置参数
        Set<String> tmpJarDirMergeFileTypeSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_JAR_DIR_MERGE_FILE_TYPE, true);
        Set<String> jarDirMergeFileTypeSet = new HashSet<>(tmpJarDirMergeFileTypeSet.size());
        for (String tmpJarDirMergeFileType : tmpJarDirMergeFileTypeSet) {
            // 文件类型使用小写
            jarDirMergeFileTypeSet.add(tmpJarDirMergeFileType.toLowerCase());
        }
        confInfo.setJarDirMergeFileTypeSet(jarDirMergeFileTypeSet);

        // 获取ignore_class_name.properties中的配置参数
        Set<String> ignoreClassNameSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_NAME, true);
        confInfo.setIgnoreClassNameSet(ignoreClassNameSet);

        // 获取ignore_jar_file_keyword.properties中的配置参数
        Set<String> ignoreJarFileKeywordSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_IGNORE_JAR_FILE_KEYWORD, true);
        confInfo.setIgnoreJarFileKeywordSet(ignoreJarFileKeywordSet);

        // 获取ignore_jar_file_name.properties中的配置参数
        Set<String> ignoreJarFileNameSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_IGNORE_JAR_FILE_NAME, true);
        confInfo.setIgnoreJarFileNameSet(ignoreJarFileNameSet);

        // 处理fr_eq_conversion_method.properties中的配置参数
        if (!handleFrEqConversionMethod(javaCG2ConfigureWrapper, confInfo)) {
            return null;
        }
        return confInfo;
    }

    // 处理fr_eq_conversion_method.properties中的配置参数
    private static boolean handleFrEqConversionMethod(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, JavaCG2ConfInfo confInfo) {
        Map<String, Map<String, Integer>> frEqConversionMethodMap = new HashMap<>();
        confInfo.setFrEqConversionMethodMap(frEqConversionMethodMap);
        Set<String> frEqConversionMethodSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, true);
        if (JavaCG2Util.isCollectionEmpty(frEqConversionMethodSet)) {
            return true;
        }
        for (String frEqConversionMethod : frEqConversionMethodSet) {
            String[] data1 = StringUtils.splitPreserveAllTokens(frEqConversionMethod, JavaCG2Constants.FLAG_EQUAL);
            if (data1.length != 2) {
                logger.error("配置文件内容不是合法的properties参数 {} {}", JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getFileName(), frEqConversionMethod);
                return false;
            }
            String classAndMethod = data1[0];
            int argObjSeq = Integer.parseInt(data1[1]);
            if (argObjSeq < 0) {
                logger.error("配置文件被调用对象（使用0表示）或方法参数（从1开始）序号非法 {} {}", JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getFileName(), frEqConversionMethod);
                return false;
            }
            String[] data2 = StringUtils.splitPreserveAllTokens(classAndMethod, JavaCG2Constants.FLAG_COLON);
            if (data2.length != 2) {
                logger.error("配置文件内容不是合法的类名与方法名 {} {}", JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getFileName(), frEqConversionMethod);
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
        return JavaCG2Util.getDirPathInJvmOptions(JavaCG2Constants.PROPERTY_INPUT_ROOT_PATH);
    }

    private JavaCG2ConfManager() {
        throw new IllegalStateException("illegal");
    }
}
