package com.adrninistrator.javacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 生成文件类型枚举
 */
public enum JavaCGOutPutFileTypeEnum {

    OPFTE_CLASS_ANNOTATION("class_annotation", "类的注解"),
    OPFTE_CLASS_INFO("class_info", "类的信息"),
    OPFTE_CLASS_NAME("class_name", "引用的类"),
    OPFTE_CLASS_SIGNATURE_EI1("class_signature_ei1", "继承父类与实现接口时的签名中的类名信息"),
    OPFTE_EXTENDS_IMPL("extends_impl", "继承与实现相关信息"),
    OPFTE_INNER_CLASS("inner_class", "内部类信息"),
    OPFTE_JAR_INFO("jar_info", "jar包信息"),
    OPFTE_LAMBDA_METHOD_INFO("lambda_method_info", "Lambda表达式方法信息"),
    OPFTE_METHOD_ANNOTATION("method_annotation", "方法的注解"),
    OPFTE_METHOD_ARG_GENERICS_TYPE("method_arg_generics_type", "方法参数泛型类型"),
    OPFTE_METHOD_CALL("method_call", "方法调用"),
    OPFTE_METHOD_CALL_INFO("method_call_info", "方法调用的信息"),
    OPFTE_METHOD_INFO("method_info", "方法的信息"),
    OPFTE_METHOD_LINE_NUMBER("method_line_number", "方法代码行号"),
    OPFTE_METHOD_RETURN_GENERICS_TYPE("method_return_generics_type", "方法返回泛型类型"),
    OPFTE_SPRING_BEAN("spring_bean", "Spring Bean信息"),
    ;

    private final String fileName;
    private final String desc;

    JavaCGOutPutFileTypeEnum(String fileName, String desc) {
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
