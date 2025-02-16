package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/4
 * @description: 配置参数管理类
 */
public class JavaCG2ConfManager {
    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ConfManager.class);

    public static JavaCG2ConfInfo getConfInfo(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper) {
        JavaCG2ConfInfo javaCG2ConfInfo = new JavaCG2ConfInfo();
        boolean parseMethodCallTypeValue = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE);
        boolean firstParseInitMethodType = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE);
        boolean analyseFieldRelationship = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP);

        if (!parseMethodCallTypeValue) {
            if (firstParseInitMethodType) {
                logger.warn("配置文件 {} 中的 {} 参数值为false时， {} 参数值不会生效-1", JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getFileName(),
                        JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey(), JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE.getKey());
                firstParseInitMethodType = false;
            }
            if (analyseFieldRelationship) {
                logger.warn("配置文件 {} 中的 {} 参数值为false时， {} 参数值不会生效-2", JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getFileName(),
                        JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey(), JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP.getKey());
                analyseFieldRelationship = false;
            }
        }

        javaCG2ConfInfo.setParseMethodCallTypeValue(parseMethodCallTypeValue);
        javaCG2ConfInfo.setFirstParseInitMethodType(firstParseInitMethodType);
        javaCG2ConfInfo.setAnalyseFieldRelationship(analyseFieldRelationship);

        // 处理 fr_eq_conversion_method.properties 中的配置参数
        if (!handleFrEqConversionMethod(javaCG2ConfigureWrapper, javaCG2ConfInfo)) {
            return null;
        }
        return javaCG2ConfInfo;
    }

    // 处理 fr_eq_conversion_method.properties 中的配置参数
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
                logger.error("配置文件内容不是合法的properties参数 {} {}", JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getKey(), frEqConversionMethod);
                return false;
            }
            String classAndMethod = data1[0];
            int argObjSeq = Integer.parseInt(data1[1]);
            if (argObjSeq < 0) {
                logger.error("配置文件被调用对象（使用0表示）或方法参数（从1开始）序号非法 {} {}", JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getKey(), frEqConversionMethod);
                return false;
            }
            String[] data2 = StringUtils.splitPreserveAllTokens(classAndMethod, JavaCG2Constants.FLAG_COLON);
            if (data2.length != 2) {
                logger.error("配置文件内容不是合法的类名与方法名 {} {}", JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD.getKey(), frEqConversionMethod);
                return false;
            }
            String className = data2[0];
            String methodName = data2[1];
            Map<String, Integer> methodMap = frEqConversionMethodMap.computeIfAbsent(className, k -> new HashMap<>());
            methodMap.put(methodName, argObjSeq);
        }
        return true;
    }

    private JavaCG2ConfManager() {
        throw new IllegalStateException("illegal");
    }
}
