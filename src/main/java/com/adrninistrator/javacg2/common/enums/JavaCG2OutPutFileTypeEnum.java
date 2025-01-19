package com.adrninistrator.javacg2.common.enums;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 生成文件类型枚举
 */
public enum JavaCG2OutPutFileTypeEnum {

    OPFTE_CLASS_ANNOTATION("class_annotation", "类的注解"),
    OPFTE_CLASS_EXT_IMPL_GENERICS_TYPE("class_ext_impl_generics_type", "类的继承或实现的泛型信息"),
    OPFTE_CLASS_INFO("class_info", "类的信息"),
    OPFTE_CLASS_REFERENCE("class_reference", "引用的类"),
    OPFTE_CLASS_SIGNATURE_GENERICS_TYPE("class_signature_generics_type", "类的签名中的泛型信息"),
    OPFTE_DUP_CLASS_INFO("dup_class_info", "重复类的信息"),
    OPFTE_DUP_METHOD_INFO("dup_method_info", "重复类的方法的信息"),
    OPFTE_ENUM_INIT_ARG_FIELD("enum_init_arg_field", "枚举类构造函数参数与字段赋值关系"),
    OPFTE_ENUM_INIT_ASSIGN_INFO("enum_init_assign_info", "枚举类初始化赋值信息"),
    OPFTE_EXTENDS_IMPL("extends_impl", "继承与实现相关信息"),
    OPFTE_FIELD_ANNOTATION("field_annotation", "字段的注解"),
    OPFTE_FIELD_GENERICS_TYPE("field_generics_type", "非静态字段集合中涉及的泛型类型"),
    OPFTE_FIELD_INFO("field_info", "字段信息"),
    OPFTE_FIELD_RELATIONSHIP("field_relationship", "通过get/set方法关联的字段关系"),
    OPFTE_GET_METHOD("get_method", "dto的get方法及字段"),
    OPFTE_INNER_CLASS("inner_class", "内部类信息"),
    OPFTE_JAR_INFO("jar_info", "jar包信息"),
    OPFTE_JAVACG2_CONFIG("javacg2_config", "java-callgraph2组件使用的配置参数"),
    OPFTE_LAMBDA_METHOD_INFO("lambda_method_info", "Lambda表达式方法信息"),
    OPFTE_LOG_METHOD_SPEND_TIME("_log_method_spend_time", "日志-方法处理耗时"),
    OPFTE_METHOD_ANNOTATION("method_annotation", "方法的注解"),
    OPFTE_METHOD_ARG_ANNOTATION("method_arg_annotation", "方法参数的注解"),
    OPFTE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", "方法参数集合中涉及的泛型类型"),
    OPFTE_METHOD_ARGUMENT("method_argument", "方法参数"),
    OPFTE_METHOD_CALL("method_call", "方法调用"),
    OPFTE_METHOD_CALL_INFO("method_call_info", "方法调用的信息"),
    OPFTE_METHOD_CALL_METHOD_CALL_RETURN("method_call_method_call_return", "方法调用使用方法调用返回值"),
    OPFTE_METHOD_CALL_STATIC_FIELD("method_call_static_field", "方法调用使用静态字段信息"),
    OPFTE_METHOD_CATCH("method_catch", "方法的catch信息"),
    OPFTE_METHOD_FINALLY("method_finally", "方法的finally信息"),
    OPFTE_METHOD_INFO("method_info", "方法的信息"),
    OPFTE_METHOD_LINE_NUMBER("method_line_number", "方法代码行号"),
    OPFTE_METHOD_RETURN_ARG_SEQ("method_return_arg_seq", "方法返回值对应的方法序号"),
    OPFTE_METHOD_RETURN_CALL_ID("method_return_call_id", "方法返回值对应的方法调用ID"),
    OPFTE_METHOD_RETURN_CONST_VALUE("method_return_const_value", "方法返回的常量值（含null）"),
    OPFTE_METHOD_RETURN_FIELD_INFO("method_return_field_info", "方法返回的字段（含枚举）"),
    OPFTE_METHOD_RETURN_GENERICS_TYPE("method_return_generics_type", "方法返回集合中涉及的泛型类型"),
    OPFTE_METHOD_THROW("method_throw", "方法通过throw抛出的异常信息"),
    OPFTE_SET_METHOD("set_method", "dto的set方法及字段"),
    OPFTE_SF_FIELD_METHOD_CALL("sf_field_method_call", "static、final字段初始化方法信息（含枚举）"),
    OPFTE_SPRING_BEAN("spring_bean", "Spring Bean信息"),
    OPFTE_ILLEGAL("illegal", "非法值"),
    ;

    private final String fileName;
    private final String desc;

    JavaCG2OutPutFileTypeEnum(String fileName, String desc) {
        this.fileName = fileName;
        this.desc = desc;
    }

    public String getFileName() {
        return fileName;
    }

    public String getDesc() {
        return desc;
    }
}
