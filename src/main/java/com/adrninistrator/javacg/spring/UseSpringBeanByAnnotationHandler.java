package com.adrninistrator.javacg.spring;

import com.adrninistrator.javacg.common.SpringAnnotationConstants;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.extensions.code_parser.SpringXmlBeanParserInterface;
import com.adrninistrator.javacg.util.JavaCGAnnotationUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.commons.lang3.StringUtils;

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

    private final Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap;

    private final DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler;

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

    // 是否有通过注解使用Spring Bean
    private boolean useSpringBean = false;

    public UseSpringBeanByAnnotationHandler(Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap,
                                            DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler,
                                            SpringXmlBeanParserInterface springXmlBeanParser) {
        this.classExtendsMethodInfoMap = classExtendsMethodInfoMap;
        this.defineSpringBeanByAnnotationHandler = defineSpringBeanByAnnotationHandler;
        this.springXmlBeanParser = springXmlBeanParser;
        if (springXmlBeanParser != null) {
            System.out.println("指定" + SpringXmlBeanParserInterface.class.getSimpleName() + "实例 " + springXmlBeanParser.getClass().getName());
        } else {
            System.out.println("未指定" + SpringXmlBeanParserInterface.class.getSimpleName() + "实例");
        }
    }

    /**
     * 记录类中带有Spring相关注解的字段信息
     *
     * @param javaClass
     */
    public void recordClassFieldsWithSpringAnnotation(JavaClass javaClass) {
        Map<String, String> fieldSpringBeanNameMap = new HashMap<>(10);
        Map<String, String> fieldSpringBeanTypeMap = new HashMap<>();

        for (Field field : javaClass.getFields()) {
            String fieldName = field.getName();
            AnnotationEntry resourceAnnotationEntry = null;
            AnnotationEntry autowiredAnnotationEntry = null;
            AnnotationEntry qualifierAnnotationEntry = null;
            AnnotationEntry namedAnnotationEntry = null;
            AnnotationEntry injectAnnotationEntry = null;

            for (AnnotationEntry annotationEntry : field.getAnnotationEntries()) {
                if (SpringAnnotationConstants.ANNOTATION_NAME_RESOURCE.equals(annotationEntry.getAnnotationType())) {
                    resourceAnnotationEntry = annotationEntry;
                } else if (SpringAnnotationConstants.ANNOTATION_NAME_AUTOWIRED.equals(annotationEntry.getAnnotationType())) {
                    autowiredAnnotationEntry = annotationEntry;
                } else if (SpringAnnotationConstants.ANNOTATION_NAME_QUALIFIER.equals(annotationEntry.getAnnotationType())) {
                    qualifierAnnotationEntry = annotationEntry;
                } else if (SpringAnnotationConstants.ANNOTATION_NAME_NAMED.equals(annotationEntry.getAnnotationType())) {
                    namedAnnotationEntry = annotationEntry;
                } else if (SpringAnnotationConstants.ANNOTATION_NAME_INJECT.equals(annotationEntry.getAnnotationType())) {
                    injectAnnotationEntry = annotationEntry;
                }
            }

            // 记录是否有处理@Resource注解的type属性，若有则不处理name属性
            boolean skip = false;
            if (resourceAnnotationEntry != null) {
                /*
                    字段上存在@Resource注解，但未获取到name属性值，再获取type属性值
                 */
                String typeAttributeValue = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(resourceAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_TYPE);
                if (typeAttributeValue != null) {
                    fieldSpringBeanTypeMap.put(fieldName, typeAttributeValue);
                    skip = true;
                }
            }

            if (!skip) {
                // 获取带有Spring注解的字段对应的Bean名称
                String beanName = getSpringBeanNameOfField(resourceAnnotationEntry, autowiredAnnotationEntry, qualifierAnnotationEntry, namedAnnotationEntry,
                        injectAnnotationEntry, fieldName);
                if (beanName != null) {
                    fieldSpringBeanNameMap.put(fieldName, beanName);
                }
            }
        }

        if (!fieldSpringBeanNameMap.isEmpty()) {
            useSpringBean = true;
            classFieldSpringBeanNameMap.put(javaClass.getClassName(), fieldSpringBeanNameMap);
        }
        if (!fieldSpringBeanTypeMap.isEmpty()) {
            useSpringBean = true;
            classFieldSpringBeanTypeMap.put(javaClass.getClassName(), fieldSpringBeanTypeMap);
        }
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
            String nameOfResource = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(resourceAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_NAME);
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
            String valueOfNamed = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(namedAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
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
        String valueOfQualifier = JavaCGAnnotationUtil.getAnnotationAttributeStringValue(qualifierAnnotationEntry, SpringAnnotationConstants.ANNOTATION_ATTRIBUTE_VALUE);
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
        List<String> springBeanTypeList = defineSpringBeanByAnnotationHandler.getSpringBeanTypeList(springBeanName);
        if (!JavaCGUtil.isCollectionEmpty(springBeanTypeList)) {
            return springBeanTypeList;
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
            ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoMap.get(currentClassName);
            if (classExtendsMethodInfo == null) {
                // 当前类不存在自定义父类
                break;
            }

            currentClassName = classExtendsMethodInfo.getSuperClassName();
            if (JavaCGUtil.isClassInJdk(currentClassName)) {
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
            ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoMap.get(currentClassName);
            if (classExtendsMethodInfo == null) {
                // 当前类不存在自定义父类
                break;
            }

            currentClassName = classExtendsMethodInfo.getSuperClassName();
            if (JavaCGUtil.isClassInJdk(currentClassName)) {
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
