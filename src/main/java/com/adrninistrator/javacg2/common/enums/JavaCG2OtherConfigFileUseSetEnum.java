package com.adrninistrator.javacg2.common.enums;

import com.adrninistrator.javacg2.common.JavaCG2Constants;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCG2OtherConfigFileUseSetEnum {
    OCFUSE_PACKAGES(JavaCG2Constants.DIR_CONFIG + "/packages.properties",
            "(作用) 指定需要处理的包名或完整类名，若不指定，则所有的包名都处理（每行代表一条记录，支持多行）"
                    + JavaCG2Constants.NEW_LINE + "(范围) 仅当处理jar包中的类时受当前配置文件控制，若处理目录中的类时不受当前配置文件控制"
                    + JavaCG2Constants.NEW_LINE + "(影响) 假如当前文件非空，且需要将多个jar包/目录合并为一个jar包，则包名/类名不在当前文件中的类【不会】被合并到新的jar包中"
                    + JavaCG2Constants.NEW_LINE + "(示例) com.test"
                    + JavaCG2Constants.NEW_LINE + "(示例) com.test.ClassName"
    ),
    OCFUSE_JAR_DIR_MERGE_FILE_TYPE(JavaCG2Constants.DIR_CONFIG + "/jar_dir_merge_file_type.properties",
            "(作用) 需要合并jar包或目录时，除了class文件外，还需要合并的文件类型（每行代表一条记录，支持多行）"
                    + JavaCG2Constants.NEW_LINE + "(格式) 文件类型需要以\".\"开头，不区分大小写，不需要指定.class"
                    + JavaCG2Constants.NEW_LINE + "(示例) .xml"
                    + JavaCG2Constants.NEW_LINE + "(示例) .properties"
    ),
    OCFUSE_IGNORE_CLASS_NAME(JavaCG2Constants.DIR_CONFIG + "/ignore_class_name.properties",
            "# (作用) 在处理jar包中的文件时，假如class文件对应类名在当前文件中，则不处理对应的类（每行代表一条记录，支持多行）"
                    + JavaCG2Constants.NEW_LINE + "# (示例) a.b.Class1"
    ),
    OCFUSE_IGNORE_JAR_FILE_KEYWORD(JavaCG2Constants.DIR_CONFIG + "/ignore_jar_file_keyword.properties",
            "(作用) 在处理jar包中的文件时，文件名在当前文件中时就忽略（每行代表一条记录，支持多行）"
                    + JavaCG2Constants.NEW_LINE + "(格式) 文件名区分大小写，需要包含文件后缀"
                    + JavaCG2Constants.NEW_LINE + "(影响) 假如当前文件非空，且需要将多个jar包/目录合并为一个jar包，需要忽略的文件【不会】被合并到新的jar包中"
                    + JavaCG2Constants.NEW_LINE + "(示例) a.class"
                    + JavaCG2Constants.NEW_LINE + "(示例) b.xml"
    ),
    OCFUSE_IGNORE_JAR_FILE_NAME(JavaCG2Constants.DIR_CONFIG + "/ignore_jar_file_name.properties",
            "(作用) 在处理jar包中的文件时，文件路径包含当前文件中的关键字时就忽略（每行代表一条记录，支持多行）"
                    + JavaCG2Constants.NEW_LINE + "(格式) 关键字可以包含斜杠，也可以不包含斜杠"
                    + JavaCG2Constants.NEW_LINE + "(影响) 假如当前文件非空，且需要将多个jar包/目录合并为一个jar包，需要忽略的文件【不会】被合并到新的jar包中"
                    + JavaCG2Constants.NEW_LINE + "(示例) keyword1"
                    + JavaCG2Constants.NEW_LINE + "(示例) keyword2/keyword3"
    ),
    OCFUSE_FR_EQ_CONVERSION_METHOD(JavaCG2Constants.DIR_CONFIG + "/fr_eq_conversion_method.properties",
            "(作用) 在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法（每行代表一条记录，支持多行）"
                    + JavaCG2Constants.NEW_LINE + "(内容) key指定对应的方法，包含{完整类名}:{方法名}"
                    + JavaCG2Constants.NEW_LINE + "(内容) value指定与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号"
                    + JavaCG2Constants.NEW_LINE + "(格式) {完整类名}:{方法名}={被调用对象或方法参数序号}"
    ),
    ;

    private final String fileName;
    private final String desc;

    JavaCG2OtherConfigFileUseSetEnum(String fileName, String desc) {
        this.fileName = fileName;
        this.desc = desc;
    }

    public String getFileName() {
        return fileName;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return fileName;
    }
}
