package com.adrninistrator.javacg.common;

/**
 * @author adrninistrator
 * @date 2022/10/3
 * @description:
 */
public class SpringAnnotationConstants {

    public static final String[] SPRING_COMPONENT_ANNOTATIONS_RAW = new String[]{
            "Lorg/springframework/stereotype/Component;",
            "Lorg/springframework/stereotype/Controller;",
            "Lorg/springframework/stereotype/Repository;",
            "Lorg/springframework/stereotype/Service;",
            "Lorg/springframework/web/bind/annotation/RestController;"
    };

    public static final String ANNOTATION_ATTRIBUTE_VALUE = "value";
    public static final String ANNOTATION_ATTRIBUTE_NAME = "name";
    public static final String ANNOTATION_ATTRIBUTE_TYPE = "type";

    public static final String ANNOTATION_NAME_RESOURCE = "Ljavax/annotation/Resource;";
    public static final String ANNOTATION_NAME_AUTOWIRED = "Lorg/springframework/beans/factory/annotation/Autowired;";
    public static final String ANNOTATION_NAME_QUALIFIER = "Lorg/springframework/beans/factory/annotation/Qualifier;";
    public static final String ANNOTATION_NAME_CONFIGURATION = "Lorg/springframework/context/annotation/Configuration;";
    public static final String ANNOTATION_NAME_BEAN = "Lorg/springframework/context/annotation/Bean;";

    private SpringAnnotationConstants() {
        throw new IllegalStateException("illegal");
    }
}
