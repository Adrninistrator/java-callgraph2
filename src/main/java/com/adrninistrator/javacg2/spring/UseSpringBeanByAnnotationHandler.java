package com.adrninistrator.javacg2.spring;

import com.adrninistrator.javacg2.common.SpringAnnotationConstants;
import com.adrninistrator.javacg2.dto.classes.ClassExtendsInfo;
import com.adrninistrator.javacg2.dto.spring.SpringBeanInJava;
import com.adrninistrator.javacg2.extensions.codeparser.SpringXmlBeanParserInterface;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/10/3
 * @description: 处理通过注解使用Spring Bean的信息
 */
public class UseSpringBeanByAnnotationHandler {

    private static final Logger logger = LoggerFactory.getLogger(UseSpringBeanByAnnotationHandler.class);

    private final Map<String, ClassExtendsInfo> classExtendsInfoMap;

    private final Map<String, List<String>> classImplementsInfoMap;

    private final Map<String, List<String>> interfaceExtendsInfoMap;

    private final SpringDefineBeanHandler springDefineBeanHandler;

    private final SpringXmlBeanParserInterface springXmlBeanParser;

    /*
        保存各个类中使用的Spring Bean字段信息
            key:    完整类名
            value:  内层Map
                    key:    字段名称
                    value:  Spring Bean名称，空字符串代表不需要转换为具体的类型；非空字段串代表转换为具体的类型
     */
    private final Map<String, Map<String, String>> classFieldSpringBeanNameMap = new HashMap<>(100);

    /*
        保存各个类中使用的Spring Bean类型信息，仅当字段使用@Resource注解的type属性时会被记录下来
            key:    完整类名
            value:  内层Map
                    key:    字段名称
                    value:  Spring Bean类型
     */
    private final Map<String, Map<String, String>> classFieldSpringBeanTypeMap = new HashMap<>();

    /*
        保存各个类中使用的@Autowired、@Inject注解的字段信息
            key:    完整类名
            value:  内层Map
                    key:    字段名称
                    value:  字段类型
     */
    private final Map<String, Map<String, String>> classAutowiredFieldInfoMap = new HashMap<>(100);

    /*
        保存各个类中使用了@Autowired、@Inject注解的字段匹配的Spring Bean类型列表
            key:    完整类名
            value:  内层Map
                    key:    字段名称
                    value:  字段匹配的Spring Bean类型列表
     */
    private final Map<String, Map<String, List<String>>> classAutowiredFieldMatchedTypeMap = new HashMap<>(100);

    // 是否有通过注解使用Spring Bean
    private boolean useSpringBean = false;

    public UseSpringBeanByAnnotationHandler(Map<String, ClassExtendsInfo> classExtendsInfoMap,
                                            Map<String, List<String>> classImplementsInfoMap,
                                            Map<String, List<String>> interfaceExtendsInfoMap,
                                            SpringDefineBeanHandler springDefineBeanHandler,
                                            SpringXmlBeanParserInterface springXmlBeanParser) {
        this.classExtendsInfoMap = classExtendsInfoMap;
        this.classImplementsInfoMap = classImplementsInfoMap;
        this.interfaceExtendsInfoMap = interfaceExtendsInfoMap;
        this.springDefineBeanHandler = springDefineBeanHandler;
        this.springXmlBeanParser = springXmlBeanParser;
        if (springXmlBeanParser != null) {
            logger.info("指定 {} 实例 {}", SpringXmlBeanParserInterface.class.getSimpleName(), springXmlBeanParser.getClass().getName());
        } else {
            logger.info("未指定 {} 实例", SpringXmlBeanParserInterface.class.getSimpleName());
        }
    }

    /**
     * 记录类中带有Spring相关注解的字段信息
     *
     * @param javaClass
     */
    public void recordClassFieldsWithSpringAnnotation(JavaClass javaClass) {
        String className = javaClass.getClassName();
        Map<String, String> fieldSpringBeanNameMap = new HashMap<>(10);
        Map<String, String> fieldSpringBeanTypeMap = new HashMap<>(2);

        for (Field field : javaClass.getFields()) {
            // 处理字段的Spring相关注解信息
            handleFieldWithSpringAnnotation(className, field, fieldSpringBeanNameMap, fieldSpringBeanTypeMap);
        }

        if (!fieldSpringBeanNameMap.isEmpty()) {
            useSpringBean = true;
            classFieldSpringBeanNameMap.put(className, fieldSpringBeanNameMap);
        }
        if (!fieldSpringBeanTypeMap.isEmpty()) {
            useSpringBean = true;
            classFieldSpringBeanTypeMap.put(className, fieldSpringBeanTypeMap);
        }
    }

    /**
     * 处理字段的Spring相关注解信息
     *
     * @param className
     * @param field
     * @param fieldSpringBeanNameMap
     * @param fieldSpringBeanTypeMap
     */
    private void handleFieldWithSpringAnnotation(String className,
                                                 Field field,
                                                 Map<String, String> fieldSpringBeanNameMap,
                                                 Map<String, String> fieldSpringBeanTypeMap) {
        String fieldName = field.getName();
        AnnotationEntry resourceAnnotationEntry = null;
        AnnotationEntry autowiredAnnotationEntry = null;
        AnnotationEntry qualifierAnnotationEntry = null;
        AnnotationEntry namedAnnotationEntry = null;
        AnnotationEntry injectAnnotationEntry = null;

        for (AnnotationEntry annotationEntry : field.getAnnotationEntries()) {
            if (SpringAnnotationConstants.ANNOTATION_TYPE_RESOURCE.equals(annotationEntry.getAnnotationType())) {
                resourceAnnotationEntry = annotationEntry;
            } else if (SpringAnnotationConstants.ANNOTATION_TYPE_AUTOWIRED.equals(annotationEntry.getAnnotationType())) {
                autowiredAnnotationEntry = annotationEntry;
            } else if (SpringAnnotationConstants.ANNOTATION_TYPE_QUALIFIER.equals(annotationEntry.getAnnotationType())) {
                qualifierAnnotationEntry = annotationEntry;
            } else if (SpringAnnotationConstants.ANNOTATION_TYPE_NAMED.equals(annotationEntry.getAnnotationType())) {
                namedAnnotationEntry = annotationEntry;
            } else if (SpringAnnotationConstants.ANNOTATION_TYPE_INJECT.equals(annotationEntry.getAnnotationType())) {
                injectAnnotationEntry = annotationEntry;
            }
        }

        if (resourceAnnotationEntry != null) {
            // 字段上存在@Resource注解
            String typeAttributeValue = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(resourceAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_TYPE);
            if (typeAttributeValue != null) {
                // @Resource注解获取到了type属性值，后续处理跳过，不处理name属性
                fieldSpringBeanTypeMap.put(fieldName, typeAttributeValue);
                return;
            }
        }

        // @Resource注解未获取到type属性值，后续处理不跳过，处理name属性
        // 获取带有Spring注解的字段对应的Bean名称
        String beanName = getSpringBeanNameOfField(resourceAnnotationEntry, autowiredAnnotationEntry, qualifierAnnotationEntry, namedAnnotationEntry,
                injectAnnotationEntry, fieldName);
        if (beanName == null) {
            // 未获取到Bean的名称，不向以下Map中添加
            return;
        }

        if ((autowiredAnnotationEntry != null && qualifierAnnotationEntry == null) || injectAnnotationEntry != null) {
            // 当前使用的是不带@Qualifier注解的@Autowired注解，或者@Inject注解
            // 记录当前的字段名称，及字段类型
            Map<String, String> autowiredFieldInfoMap = classAutowiredFieldInfoMap.computeIfAbsent(className, k -> new HashMap<>());
            autowiredFieldInfoMap.put(fieldName, field.getType().toString());
        }

        // 记录字段的名称及对应的Bean的名称
        fieldSpringBeanNameMap.put(fieldName, beanName);
    }

    /**
     * 获取带有Spring注解的字段对应的Bean名称
     *
     * @param resourceAnnotationEntry
     * @param autowiredAnnotationEntry
     * @param qualifierAnnotationEntry
     * @param namedAnnotationEntry
     * @param injectAnnotationEntry
     * @param fieldName
     * @return
     */
    private String getSpringBeanNameOfField(AnnotationEntry resourceAnnotationEntry,
                                            AnnotationEntry autowiredAnnotationEntry,
                                            AnnotationEntry qualifierAnnotationEntry,
                                            AnnotationEntry namedAnnotationEntry,
                                            AnnotationEntry injectAnnotationEntry,
                                            String fieldName) {
        if (resourceAnnotationEntry == null && autowiredAnnotationEntry == null && injectAnnotationEntry == null) {
            // 字段上没有@Resource、@Autowired、@Inject注解
            // 返回null，代表字段不是Spring Bean
            return null;
        }

        if (resourceAnnotationEntry != null) {
            // 字段上有@Resource注解，获取name属性值
            String nameOfResource = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(resourceAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_NAME);
            if (nameOfResource == null) {
                // @Resource注解name属性值为空，返回字段名称
                return fieldName;
            }
            return nameOfResource;
        }

        if (injectAnnotationEntry != null) {
            // 字段上有@Inject注解
            if (namedAnnotationEntry == null) {
                // 字段上没有@Named注解，返回字段名称
                return fieldName;
            }
            // 字段上有@Named注解
            String valueOfNamed = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(namedAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
            if (StringUtils.isBlank(valueOfNamed)) {
                // @Named注解的value属性值为空，返回字段名称
                return fieldName;
            }
            // @Named注解的value属性值非空，返回value属性值
            return valueOfNamed;
        }

        // 字段上有@Autowired注解
        if (qualifierAnnotationEntry == null) {
            // 字段上没有@Qualifier注解，返回字段名称
            return fieldName;
        }

        // 字段上有@Qualifier注解
        String valueOfQualifier = JavaCG2AnnotationUtil.getAnnotationAttributeStringValue(qualifierAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
        if (StringUtils.isBlank(valueOfQualifier)) {
            // @Qualifier注解的value属性值为空，返回字段名称
            return fieldName;
        }
        // @Qualifier注解的value属性值非空，返回value属性值
        return valueOfQualifier;
    }

    /**
     * 获取指定类指定字段对应的Spring Bean类型列表
     *
     * @param className
     * @param fieldName
     * @return
     */
    public List<String> getSpringBeanTypeList(String className, String fieldName) {
        // 获取指定类指定字段对应的Spring Bean的类型，通过注解中的type属性获取
        String springBeanType = getSpringBeanTypeByAnnotationType(className, fieldName);
        if (springBeanType != null) {
            return Collections.singletonList(springBeanType);
        }

        // 获取指定类指定字段对应的Spring Bean名称
        String springBeanName = getSpringBeanName(className, fieldName);
        if (springBeanName == null) {
            return Collections.emptyList();
        }

        // 获取指定类指定字段对应的Spring Bean类型列表
        SpringBeanInJava springBeanInJava = springDefineBeanHandler.getSpringBeanInJavaMap().get(springBeanName);
        if (springBeanInJava != null) {
            // 检查@Autowired、@Inject注解的字段类型与Spring Bean类型是否匹配（使用@Autowired注解时未指定@Qualifier注解）
            return checkAutowiredTypeMatches(className, fieldName, springBeanInJava.getClassNameList());
        }

        if (springXmlBeanParser != null) {
            // 从Spring XML中获取bean类型
            String springBeanTypeInXml = springXmlBeanParser.getBeanClass(springBeanName);
            if (springBeanTypeInXml != null) {
                return Collections.singletonList(springBeanTypeInXml);
            }
        }
        return Collections.emptyList();
    }

    /**
     * 检查@Autowired、@Inject注解的字段类型与Spring Bean类型是否匹配（使用@Autowired注解时未指定@Qualifier注解）
     *
     * @param className
     * @param fieldName
     * @param springBeanTypeList
     * @return 匹配的Spring Bean类型列表
     */
    private List<String> checkAutowiredTypeMatches(String className, String fieldName, List<String> springBeanTypeList) {
        Map<String, String> autowiredFieldInfoMap = classAutowiredFieldInfoMap.get(className);
        if (autowiredFieldInfoMap == null) {
            // 当前类中没有@Autowired、@Inject注解的字段
            return springBeanTypeList;
        }

        // 判断当前字段是否有@Autowired、@Inject注解
        String fieldType = autowiredFieldInfoMap.get(fieldName);
        if (fieldType == null) {
            // 当前字段没有@Autowired、@Inject注解
            return springBeanTypeList;
        }

        // 判断当前类的当前字段匹配的Spring Bean类型是否有处理过，若有则直接使用
        Map<String, List<String>> autowiredFieldMatchedTypeMap = classAutowiredFieldMatchedTypeMap.computeIfAbsent(className, k -> new HashMap<>());
        List<String> cachedMatchedTypeList = autowiredFieldMatchedTypeMap.get(fieldName);
        if (cachedMatchedTypeList != null) {
            return cachedMatchedTypeList;
        }

        List<String> matchedSpringBeanTypeList = new ArrayList<>(springBeanTypeList.size());
        for (String springBeanType : springBeanTypeList) {
            if (springBeanType.equals(fieldType) ||
                    JavaCG2ClassMethodUtil.isChildOf(springBeanType, fieldType, classExtendsInfoMap) ||
                    JavaCG2ClassMethodUtil.isImplementationOf(springBeanType, fieldType, classExtendsInfoMap, classImplementsInfoMap, interfaceExtendsInfoMap)) {
                /*
                    若满足以下任意条件，则认为类型匹配
                    当前字段的类型=Spring Bean类型
                    Spring Bean类型是当前字段的类型的子类
                    Spring Bean类型是当前字段的类型的实现类
                 */
                matchedSpringBeanTypeList.add(springBeanType);
                continue;
            }

            // 字段类型，与字段对应的Spring Bean的类型不匹配
            logger.warn("以下类获取到的字段的Spring Bean类型与字段类型不匹配 {} {} {} {}", className, fieldName, springBeanType, fieldType);
        }

        // 记录当前类的当前字段匹配的Spring Bean类型
        autowiredFieldMatchedTypeMap.put(fieldName, matchedSpringBeanTypeList);
        return matchedSpringBeanTypeList;
    }

    /**
     * 获取指定类指定字段对应的Spring Bean的类型，通过注解中的type属性获取
     *
     * @param className
     * @param fieldName
     * @return
     */
    private String getSpringBeanTypeByAnnotationType(String className, String fieldName) {
        // 先从当前类的字段中获取
        String springBeanFieldType = doGetSpringBeanTypeByAnnotationType(className, fieldName);
        if (springBeanFieldType != null) {
            return springBeanFieldType;
        }

        // 尝试从当前类的父类字段中获取
        String currentClassName = className;
        while (true) {
            ClassExtendsInfo classExtendsInfo = classExtendsInfoMap.get(currentClassName);
            if (classExtendsInfo == null) {
                // 当前类不存在自定义父类
                break;
            }

            currentClassName = classExtendsInfo.getSuperClassName();
            if (JavaCG2ClassMethodUtil.isClassInJdk(currentClassName)) {
                // 当前类父类为JDK中的类
                break;
            }

            // 从当前类的父类字段中获取
            springBeanFieldType = doGetSpringBeanTypeByAnnotationType(currentClassName, fieldName);
            if (springBeanFieldType != null) {
                // 假如当前字段在父类中，且为Spring Bean，则添加到当前类的Map中，避免再到父类Map中查找
                Map<String, String> fieldSpringBeanTypeMap = classFieldSpringBeanTypeMap.computeIfAbsent(className, k -> new HashMap<>());
                fieldSpringBeanTypeMap.put(fieldName, springBeanFieldType);
                break;
            }
        }
        return springBeanFieldType;
    }

    /**
     * 执行获取指定类指定字段对应的Spring Bean类型，通过注解中的type属性获取
     *
     * @param className 类名
     * @param fieldName 字段名
     * @return
     */
    private String doGetSpringBeanTypeByAnnotationType(String className, String fieldName) {
        // 获取类对应的字段及Spring Bean类型Map
        Map<String, String> fieldSpringBeanTypeMap = classFieldSpringBeanTypeMap.get(className);
        if (fieldSpringBeanTypeMap == null) {
            return null;
        }
        return fieldSpringBeanTypeMap.get(fieldName);
    }

    /**
     * 获取指定类指定字段对应的Spring Bean名称
     *
     * @param className
     * @param fieldName
     * @return
     */
    private String getSpringBeanName(String className, String fieldName) {
        // 先从当前类的字段中获取
        String springBeanFieldName = doGetSpringBeanName(className, fieldName);
        if (springBeanFieldName != null) {
            return springBeanFieldName;
        }

        // 尝试从当前类的父类字段中获取
        String currentClassName = className;
        while (true) {
            ClassExtendsInfo ClassExtendsInfo = classExtendsInfoMap.get(currentClassName);
            if (ClassExtendsInfo == null) {
                // 当前类不存在自定义父类
                break;
            }

            currentClassName = ClassExtendsInfo.getSuperClassName();
            if (JavaCG2ClassMethodUtil.isClassInJdk(currentClassName)) {
                // 当前类父类为JDK中的类
                break;
            }

            // 从当前类的父类字段中获取
            springBeanFieldName = doGetSpringBeanName(currentClassName, fieldName);
            if (springBeanFieldName != null) {
                // 假如当前字段在父类中，且为Spring Bean，则添加到当前类的Map中，避免再到父类Map中查找
                Map<String, String> fieldSpringBeanNameMap = classFieldSpringBeanNameMap.computeIfAbsent(className, k -> new HashMap<>());
                fieldSpringBeanNameMap.put(fieldName, springBeanFieldName);
                break;
            }
        }
        return springBeanFieldName;
    }

    /**
     * 执行获取指定类指定字段对应的Spring Bean名称
     *
     * @param className 类名
     * @param fieldName 字段名
     * @return
     */
    private String doGetSpringBeanName(String className, String fieldName) {
        // 获取类对应的字段及Spring Bean名称Map
        Map<String, String> fieldSpringBeanNameMap = classFieldSpringBeanNameMap.get(className);
        if (fieldSpringBeanNameMap == null) {
            return null;
        }

        // 获取Spring Bean名称
        return fieldSpringBeanNameMap.get(fieldName);
    }

    public boolean hasUseSpringBean() {
        return useSpringBean;
    }
}
