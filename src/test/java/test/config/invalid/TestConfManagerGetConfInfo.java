package test.config.invalid;

import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeRawActualEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfInfo;
import com.adrninistrator.javacg2.conf.JavaCG2ConfManager;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2ConfigException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2026/5/24
 * @description: 验证JavaCG2ConfManager.getConfInfo方法对于不同配置参数与场景的处理是否正确
 */
public class TestConfManagerGetConfInfo {

    private static final Logger logger = LoggerFactory.getLogger(TestConfManagerGetConfInfo.class);

    private JavaCG2ConfigureWrapper configureWrapper;

    @Before
    public void init() {
        configureWrapper = new JavaCG2ConfigureWrapper();
    }

    // ===== parseMethodCallTypeValue 相关测试 =====

    // 测试 parseMethodCallTypeValue=true，firstParseInitMethodType=true，analyseFieldRelationship=true，参数都生效
    @Test
    public void testGetConfInfo_AllEnabled() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, "true");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, "true");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, "true");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isParseMethodCallTypeValue());
        Assert.assertTrue(confInfo.isFirstParseInitMethodType());
        Assert.assertTrue(confInfo.isAnalyseFieldRelationship());
    }

    // 测试 parseMethodCallTypeValue=false，firstParseInitMethodType=true 被覆盖为 false
    @Test
    public void testGetConfInfo_ParseMethodCallTypeFalse_FirstParseInitTrue() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, "false");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, "true");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, "false");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isParseMethodCallTypeValue());
        Assert.assertFalse(confInfo.isFirstParseInitMethodType());
    }

    // 测试 parseMethodCallTypeValue=false，analyseFieldRelationship=true 被覆盖为 false
    @Test
    public void testGetConfInfo_ParseMethodCallTypeFalse_AnalyseFieldTrue() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, "false");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, "false");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, "true");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isParseMethodCallTypeValue());
        Assert.assertFalse(confInfo.isAnalyseFieldRelationship());
    }

    // 测试 parseMethodCallTypeValue=false，firstParseInitMethodType=true，analyseFieldRelationship=true，两个都被覆盖为 false
    @Test
    public void testGetConfInfo_ParseMethodCallTypeFalse_BothOverride() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, "false");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, "true");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, "true");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isParseMethodCallTypeValue());
        Assert.assertFalse(confInfo.isFirstParseInitMethodType());
        Assert.assertFalse(confInfo.isAnalyseFieldRelationship());
    }

    // 测试 parseMethodCallTypeValue=false，firstParseInitMethodType=false，analyseFieldRelationship=false，不覆盖
    @Test
    public void testGetConfInfo_ParseMethodCallTypeFalse_NoOverride() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, "false");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, "false");
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, "false");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isParseMethodCallTypeValue());
        Assert.assertFalse(confInfo.isFirstParseInitMethodType());
        Assert.assertFalse(confInfo.isAnalyseFieldRelationship());
    }

    // ===== handleCalleeNewRawActual 相关测试 =====

    // 测试 handleCalleeNewRawActual=only_raw
    @Test
    public void testGetConfInfo_CalleeNewRawActual_OnlyRaw() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_ONLY_RAW.getType());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isHandleCalleeNewRaw());
        Assert.assertFalse(confInfo.isHandleCalleeNewActual());
    }

    // 测试 handleCalleeNewRawActual=only_actual
    @Test
    public void testGetConfInfo_CalleeNewRawActual_OnlyActual() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_ONLY_ACTUAL.getType());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isHandleCalleeNewRaw());
        Assert.assertTrue(confInfo.isHandleCalleeNewActual());
    }

    // 测试 handleCalleeNewRawActual=raw_actual
    @Test
    public void testGetConfInfo_CalleeNewRawActual_RawActual() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_RAW_ACTUAL.getType());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isHandleCalleeNewRaw());
        Assert.assertTrue(confInfo.isHandleCalleeNewActual());
    }

    // ===== handleCalleeSpringBeanRawActual 相关测试 =====

    // 测试 handleCalleeSpringBeanRawActual=only_raw
    @Test
    public void testGetConfInfo_CalleeSpringBeanRawActual_OnlyRaw() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_ONLY_RAW.getType());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isHandleCalleeSpringBeanRaw());
        Assert.assertFalse(confInfo.isHandleCalleeSpringBeanActual());
    }

    // 测试 handleCalleeSpringBeanRawActual=only_actual
    @Test
    public void testGetConfInfo_CalleeSpringBeanRawActual_OnlyActual() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_ONLY_ACTUAL.getType());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isHandleCalleeSpringBeanRaw());
        Assert.assertTrue(confInfo.isHandleCalleeSpringBeanActual());
    }

    // 测试 handleCalleeSpringBeanRawActual=raw_actual
    @Test
    public void testGetConfInfo_CalleeSpringBeanRawActual_RawActual() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL, JavaCG2CalleeRawActualEnum.CRAE_RAW_ACTUAL.getType());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isHandleCalleeSpringBeanRaw());
        Assert.assertTrue(confInfo.isHandleCalleeSpringBeanActual());
    }

    // ===== handleFrEqConversionMethod 相关测试 =====

    // 测试 fr_eq_conversion_method 为空
    @Test
    public void testGetConfInfo_FrEqConversionMethod_Empty() {
        configureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, new HashSet<>());

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.getFrEqConversionMethodMap().isEmpty());
    }

    // 测试 fr_eq_conversion_method 合法值
    @Test
    public void testGetConfInfo_FrEqConversionMethod_Valid() {
        Set<String> configSet = new HashSet<>();
        configSet.add("com.test.Class1:method1=1");
        configSet.add("com.test.Class2:method2=0");
        configureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, configSet);

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Map<String, Map<String, Integer>> map = confInfo.getFrEqConversionMethodMap();
        Assert.assertEquals(2, map.size());
        Assert.assertEquals(Integer.valueOf(1), map.get("com.test.Class1").get("method1"));
        Assert.assertEquals(Integer.valueOf(0), map.get("com.test.Class2").get("method2"));
    }

    // 测试 fr_eq_conversion_method 非法值-不是合法的properties参数（缺少=）
    @Test
    public void testGetConfInfo_FrEqConversionMethod_InvalidNoEqual() {
        Set<String> configSet = new HashSet<>();
        configSet.add("com.test.Class1:method1");
        configureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, configSet);

        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> JavaCG2ConfManager.getConfInfo(configureWrapper));
        logger.error("error ", e);
    }

    // 测试 fr_eq_conversion_method 非法值-不是合法的类名与方法名（缺少:）
    @Test
    public void testGetConfInfo_FrEqConversionMethod_InvalidNoColon() {
        Set<String> configSet = new HashSet<>();
        configSet.add("com.test.Class1method1=1");
        configureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, configSet);

        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> JavaCG2ConfManager.getConfInfo(configureWrapper));
        logger.error("error ", e);
    }

    // 测试 fr_eq_conversion_method 非法值-序号为负数
    @Test
    public void testGetConfInfo_FrEqConversionMethod_InvalidNegativeSeq() {
        Set<String> configSet = new HashSet<>();
        configSet.add("com.test.Class1:method1=-1");
        configureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, configSet);

        JavaCG2ConfigException e = Assert.assertThrows(JavaCG2ConfigException.class,
                () -> JavaCG2ConfManager.getConfInfo(configureWrapper));
        logger.error("error ", e);
    }

    // 测试 fr_eq_conversion_method 序号为0（合法-代表被调用对象）
    @Test
    public void testGetConfInfo_FrEqConversionMethod_SeqZero() {
        Set<String> configSet = new HashSet<>();
        configSet.add("com.test.Class1:method1=0");
        configureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD, configSet);

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertEquals(Integer.valueOf(0), confInfo.getFrEqConversionMethodMap().get("com.test.Class1").get("method1"));
    }

    // ===== parseJarCompatibilityMode 相关测试 =====

    @Test
    public void testGetConfInfo_ParseJarCompatibilityMode_True() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_JAR_COMPATIBILITY_MODE, "true");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isParseJarCompatibilityMode());
    }

    @Test
    public void testGetConfInfo_ParseJarCompatibilityMode_False() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_JAR_COMPATIBILITY_MODE, "false");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isParseJarCompatibilityMode());
    }

    // ===== parseOnlyClassMode 相关测试 =====

    @Test
    public void testGetConfInfo_ParseOnlyClassMode_True() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_ONLY_CLASS_MODE, "true");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isParseOnlyClassMode());
    }

    @Test
    public void testGetConfInfo_ParseOnlyClassMode_False() {
        configureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_ONLY_CLASS_MODE, "false");

        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertFalse(confInfo.isParseOnlyClassMode());
    }

    // ===== 默认值测试 =====

    // 测试使用默认配置参数
    @Test
    public void testGetConfInfo_DefaultValues() {
        JavaCG2ConfInfo confInfo = JavaCG2ConfManager.getConfInfo(configureWrapper);
        Assert.assertTrue(confInfo.isParseMethodCallTypeValue());
        Assert.assertTrue(confInfo.isFirstParseInitMethodType());
        Assert.assertFalse(confInfo.isAnalyseFieldRelationship());
        Assert.assertFalse(confInfo.isHandleCalleeNewRaw());
        Assert.assertTrue(confInfo.isHandleCalleeNewActual());
        Assert.assertFalse(confInfo.isHandleCalleeSpringBeanRaw());
        Assert.assertTrue(confInfo.isHandleCalleeSpringBeanActual());
        Assert.assertFalse(confInfo.isParseJarCompatibilityMode());
        Assert.assertFalse(confInfo.isParseOnlyClassMode());
    }
}
