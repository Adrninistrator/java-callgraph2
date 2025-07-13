package com.adrninistrator.javacg2.common;

/**
 * @author adrninistrator
 * @date 2022/10/3
 * @description:
 */
public class SpringAnnotationConstants {

    public static final String[] ANNOTATION_TYPE_COMPONENTS = new String[]{
            "Lorg/springframework/stereotype/Component;",
            "Lorg/springframework/stereotype/Controller;",
            "Lorg/springframework/stereotype/Repository;",
            "Lorg/springframework/stereotype/Service;",
            "Lorg/springframework/web/bind/annotation/RestController;"
    };

    public static final String ANNOTATION_ATTRIBUTE_VALUE = "value";
    public static final String ANNOTATION_ATTRIBUTE_NAME = "name";
    public static final String ANNOTATION_ATTRIBUTE_TYPE = "type";
    public static final String ANNOTATION_ATTRIBUTE_BASE_PACKAGES = "basePackages";
    public static final String ANNOTATION_ATTRIBUTE_POINTCUT = "pointcut";

    public static final String ANNOTATION_TYPE_RESOURCE = "Ljavax/annotation/Resource;";
    public static final String ANNOTATION_TYPE_AUTOWIRED = "Lorg/springframework/beans/factory/annotation/Autowired;";
    public static final String ANNOTATION_TYPE_QUALIFIER = "Lorg/springframework/beans/factory/annotation/Qualifier;";
    public static final String ANNOTATION_TYPE_CONFIGURATION = "Lorg/springframework/context/annotation/Configuration;";
    public static final String ANNOTATION_TYPE_BEAN = "Lorg/springframework/context/annotation/Bean;";
    public static final String ANNOTATION_TYPE_COMPONENT_SCAN = "Lorg/springframework/context/annotation/ComponentScan;";
    public static final String ANNOTATION_TYPE_ASPECT = "Lorg/aspectj/lang/annotation/Aspect;";
    public static final String ANNOTATION_TYPE_NAMED = "Ljavax/inject/Named;";
    public static final String ANNOTATION_TYPE_INJECT = "Ljavax/inject/Inject;";

    public static final String ANNOTATION_TYPE_ORDER = "Lorg/springframework/core/annotation/Order;";

    public static final String[] ANNOTATION_TYPE_ADVICES = new String[]{
            "Lorg/aspectj/lang/annotation/AfterReturning;",
            "Lorg/aspectj/lang/annotation/AfterThrowing;",
            "Lorg/aspectj/lang/annotation/After;",
            "Lorg/aspectj/lang/annotation/Around;",
            "Lorg/aspectj/lang/annotation/Before;",
    };

    public static final String ANNOTATION_TYPE_POINTCUT = "Lorg/aspectj/lang/annotation/Pointcut;";

    public static final String ANNOTATION_CLASS_NAME_BEAN = "org.springframework.context.annotation.Bean";

    public static final String ANNOTATION_SIMPLE_CLASS_NAME_AFTER_RETURNING = "AfterReturning";
    public static final String ANNOTATION_SIMPLE_CLASS_NAME_AFTER_THROWING = "AfterThrowing";
    public static final String ANNOTATION_SIMPLE_CLASS_NAME_AFTER = "After";

    public static final String ANNOTATION_SIMPLE_CLASS_NAME_AROUND = "Around";
    public static final String ANNOTATION_SIMPLE_CLASS_NAME_BEFORE = "Before";


    private SpringAnnotationConstants() {
        throw new IllegalStateException("illegal");
    }
}
