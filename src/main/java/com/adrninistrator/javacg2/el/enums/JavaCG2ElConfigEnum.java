package com.adrninistrator.javacg2.el.enums;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.el.checker.AbstractElChecker;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4HandleSpringBean;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MergeFileInDir;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MergeFileInJarWar;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MethodCall;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4ParseClass;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4ParseMethod;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;

/**
 * @author adrninistrator
 * @date 2025/1/28
 * @description: 使用表达式的配置文件枚举
 */
public enum JavaCG2ElConfigEnum implements ElConfigInterface {
    ECE_EXAMPLE(JavaCG2DirEnum.IDE_EL_EXAMPLE.getDirName() + "/el_usage" + JavaCG2Constants.EXT_MD, new String[]{"表达式示例文件"}, null, null, false),
    ECE_MERGE_FILE_IGNORE_JAR_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_in_dir.av",
            new String[]{"指定是否跳过合并目录中的jar文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_WAR_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_war_in_dir.av",
            new String[]{"指定是否跳过合并目录中的war文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_class_in_dir.av",
            new String[]{"指定是否跳过合并目录中的class文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_other_in_dir.av",
            new String[]{"指定是否跳过合并目录中的非jar、war、class文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_in_jar_war.av",
            new String[]{"指定是否跳过合并jar、war中的jar文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_class_in_jar_war.av",
            new String[]{"指定是否跳过合并jar、war中的class文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_other_in_jar_war.av",
            new String[]{"指定是否跳过合并jar、war中的非jar、war、class文件"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_war_by_class_dir_prefix.av",
            new String[]{"通过class文件对应指定层级的目录路径判断是否跳过合并当前jar、war文件",
                    "相当于通过jar、war文件中类的包名控制是否跳过合并当前jar、war文件",
                    "以下参数为jar、war文件中的class文件对应指定层级的目录路径集合。在表达式中可通过“include”方法判断集合中是否包含指定元素",
                    "集合中的元素类型为字符串，以/作为分隔符，不会以分隔符开头或结尾",
                    "例如jar文件中有a1/c1.class、a2/b2/c2.class，则该jar文件中的class文件目录1级路径有a1、a2，2级路径有a2/b2，没有层级大于2级的路径",
                    "在使用以下 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + " 参数时，需要以 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + " 开头，后续通过数字指定class文件路径层级",
                    "例如 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "3 代表第3级class文件路径集合"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_PARSE_IGNORE_CLASS(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName() + "/parse_ignore_class.av",
            new String[]{"指定是否跳过解析类"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME
            }, JavaCG2ElChecker4ParseClass.class, true),
    ECE_PARSE_IGNORE_METHOD(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName() + "/parse_ignore_method.av",
            new String[]{"指定是否跳过解析方法"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD
            }, JavaCG2ElChecker4ParseMethod.class, true),
    ECE_PARSE_IGNORE_METHOD_CALL(JavaCG2DirEnum.IDE_PARSE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call.av",
            new String[]{"指定是否跳过解析方法调用，支持通过方法调用类型、调用方法或被调用方法等判断"},
            new ElAllowedVariableInterface[]{
                    CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM,
                    CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD
            }, JavaCG2ElChecker4MethodCall.class, true),
    ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER(JavaCG2DirEnum.IDE_PARSE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_type_value_caller.av",
            new String[]{"指定解析方法调用被调用对象和参数可能的类型与值需要跳过哪些方法，通过调用方法判断"},
            new ElAllowedVariableInterface[]{
                    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM,
                    CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD
            }, JavaCG2ElChecker4ParseMethod.class, true),
    ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML(JavaCG2DirEnum.IDE_HANDLE_XML_SWITCH.getDirName() + "/handle_ignore_spring_bean_in_xml.av",
            new String[]{"指定处理XML文件中定义的Spring Bean需要跳过哪些Bean"},
            new ElAllowedVariableInterface[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE,
            }, JavaCG2ElChecker4HandleSpringBean.class, true),
    ;

    // 配置文件名称
    private final String fileName;

    // 配置文件描述
    private final String[] descriptions;

    // 配置文件允许使用的表达式变量枚举
    private final ElAllowedVariableInterface[] elAllowedVariableEnums;

    // 用于提前执行表达式进行检查的类
    private final Class<? extends AbstractElChecker> elCheckClass;

    // 当前参数是否用于跳过数据
    private final boolean ignoreData;

    JavaCG2ElConfigEnum(String fileName, String[] descriptions, ElAllowedVariableInterface[] elAllowedVariableEnums, Class<? extends AbstractElChecker> elCheckClass,
                        boolean ignoreData) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.elAllowedVariableEnums = elAllowedVariableEnums;
        this.elCheckClass = elCheckClass;
        this.ignoreData = ignoreData;
    }

    @Override
    public String getEnumConstantName() {
        return name();
    }

    @Override
    public String getKey() {
        return fileName;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public Class<? extends AbstractElChecker> getElCheckClass() {
        return elCheckClass;
    }

    @Override
    public boolean isIgnoreData() {
        return ignoreData;
    }

    @Override
    public String getConfigPrintInfo() {
        return fileName + " " + JavaCG2ElConfigEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public ElAllowedVariableInterface[] getElAllowedVariableEnums() {
        return elAllowedVariableEnums;
    }
}
