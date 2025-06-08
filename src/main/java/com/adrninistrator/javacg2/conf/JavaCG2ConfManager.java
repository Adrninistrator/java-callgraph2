package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeRawActualEnum;
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
        javaCG2ConfInfo.setParseMethodCallTypeValue(parseMethodCallTypeValue);

        boolean firstParseInitMethodType = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE);
        boolean analyseFieldRelationship = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP);

        if (!parseMethodCallTypeValue) {
            if (firstParseInitMethodType) {
                String logContent = String.format("配置文件 %s 中的 %s 参数值为false时， %s 参数值不会生效-1", JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getFileName(),
                        JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey(), JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE.getKey());
                logger.warn("{}", logContent);
                firstParseInitMethodType = false;
            }
            if (analyseFieldRelationship) {
                String logContent = String.format("配置文件 %s 中的 %s 参数值为false时， %s 参数值不会生效-2", JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getFileName(),
                        JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey(), JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP.getKey());
                logger.warn("{}", logContent);
                analyseFieldRelationship = false;
            }
        }

        javaCG2ConfInfo.setFirstParseInitMethodType(firstParseInitMethodType);
        javaCG2ConfInfo.setAnalyseFieldRelationship(analyseFieldRelationship);

        // 处理解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型
        if (!handleCalleeNewRawActual(javaCG2ConfigureWrapper, javaCG2ConfInfo)) {
            return null;
        }

        // 处理解析方法调用时，被调用对象为Spring Bean，类型使用原始类型还是实际类型（支持字段注入）
        if (!handleCalleeSpringBeanRawActual(javaCG2ConfigureWrapper, javaCG2ConfInfo)) {
            return null;
        }

        // 处理 fr_eq_conversion_method.properties 中的配置参数
        if (!handleFrEqConversionMethod(javaCG2ConfigureWrapper, javaCG2ConfInfo)) {
            return null;
        }

        return javaCG2ConfInfo;
    }

    // 处理 fr_eq_conversion_method.properties 中的配置参数
    private static boolean handleFrEqConversionMethod(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, JavaCG2ConfInfo javaCG2ConfInfo) {
        Map<String, Map<String, Integer>> frEqConversionMethodMap = new HashMap<>();
        javaCG2ConfInfo.setFrEqConversionMethodMap(frEqConversionMethodMap);
        Set<String> frEqConversionMethodSet = javaCG2ConfigureWrapper.getOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, true);
        if (JavaCG2Util.isCollectionEmpty(frEqConversionMethodSet)) {
            return true;
        }
        for (String frEqConversionMethod : frEqConversionMethodSet) {
            String[] data1 = StringUtils.splitPreserveAllTokens(frEqConversionMethod, JavaCG2Constants.FLAG_EQUAL);
            if (data1.length != 2) {
                logger.error("配置文件内容不是合法的properties参数 {} {}", frEqConversionMethod,
                        javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD));
                return false;
            }
            String classAndMethod = data1[0];
            int argObjSeq = Integer.parseInt(data1[1]);
            if (argObjSeq < 0) {
                logger.error("配置文件被调用对象（使用0表示）或方法参数（从1开始）序号非法 {} {}", frEqConversionMethod,
                        javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD));
                return false;
            }
            String[] data2 = StringUtils.splitPreserveAllTokens(classAndMethod, JavaCG2Constants.FLAG_COLON);
            if (data2.length != 2) {
                logger.error("配置文件内容不是合法的类名与方法名 {} {}", frEqConversionMethod,
                        javaCG2ConfigureWrapper.genConfigUsage(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD));
                return false;
            }
            String className = data2[0];
            String methodName = data2[1];
            Map<String, Integer> methodMap = frEqConversionMethodMap.computeIfAbsent(className, k -> new HashMap<>());
            methodMap.put(methodName, argObjSeq);
        }
        return true;
    }

    // 处理解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型
    private static boolean handleCalleeNewRawActual(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, JavaCG2ConfInfo javaCG2ConfInfo) {
        String handleCalleeNewRawActualStr = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL);
        JavaCG2CalleeRawActualEnum calleeNewRawActual = JavaCG2CalleeRawActualEnum.getFromType(handleCalleeNewRawActualStr);
        if (calleeNewRawActual == null) {
            logger.error("参数值非法，允许使用的值为 {} {}", JavaCG2CalleeRawActualEnum.getAllInfo(),
                    javaCG2ConfigureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL));
            return false;
        }
        switch (calleeNewRawActual) {
            case CRAE_ONLY_RAW:
                javaCG2ConfInfo.setHandleCalleeNewRaw(true);
                javaCG2ConfInfo.setHandleCalleeNewActual(false);
                break;
            case CRAE_ONLY_ACTUAL:
                javaCG2ConfInfo.setHandleCalleeNewRaw(false);
                javaCG2ConfInfo.setHandleCalleeNewActual(true);
                break;
            case CRAE_RAW_ACTUAL:
                javaCG2ConfInfo.setHandleCalleeNewRaw(true);
                javaCG2ConfInfo.setHandleCalleeNewActual(true);
                break;
        }
        return true;
    }

    // 处理解析方法调用时，被调用对象为Spring Bean，类型使用原始类型还是实际类型（支持字段注入）
    private static boolean handleCalleeSpringBeanRawActual(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, JavaCG2ConfInfo javaCG2ConfInfo) {
        String handleCalleeSpringBeanRawActualStr = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL);
        JavaCG2CalleeRawActualEnum springBeanCalleeNewRawActual = JavaCG2CalleeRawActualEnum.getFromType(handleCalleeSpringBeanRawActualStr);
        if (springBeanCalleeNewRawActual == null) {
            logger.error("参数值非法，允许使用的值为： {} {}", JavaCG2CalleeRawActualEnum.getAllInfo(),
                    javaCG2ConfigureWrapper.genConfigUsage(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL));
            return false;
        }
        switch (springBeanCalleeNewRawActual) {
            case CRAE_ONLY_RAW:
                javaCG2ConfInfo.setHandleCalleeSpringBeanRaw(true);
                javaCG2ConfInfo.setHandleCalleeSpringBeanActual(false);
                break;
            case CRAE_ONLY_ACTUAL:
                javaCG2ConfInfo.setHandleCalleeSpringBeanRaw(false);
                javaCG2ConfInfo.setHandleCalleeSpringBeanActual(true);
                break;
            case CRAE_RAW_ACTUAL:
                javaCG2ConfInfo.setHandleCalleeSpringBeanRaw(true);
                javaCG2ConfInfo.setHandleCalleeSpringBeanActual(true);
                break;
        }
        return true;
    }

    private JavaCG2ConfManager() {
        throw new IllegalStateException("illegal");
    }
}
