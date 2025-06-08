package com.adrninistrator.javacg2.el.enums;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.el.checker.ElChecker;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MergeFileInDir;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MergeFileInJarWar;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MethodCallEe;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MethodCallEr;
import com.adrninistrator.javacg2.el.checker.JavaCG2ElChecker4MethodCallErEe;
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
    ECE_EXAMPLE("el_example" + JavaCG2Constants.EXT_MD, new String[]{"表达式示例文件"}, null, null, false),
    ECE_MERGE_FILE_IGNORE_JAR_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_in_dir.av",
            new String[]{"指定是否跳过合并目录中的jar文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_WAR_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_war_in_dir.av",
            new String[]{"指定是否跳过合并目录中的war文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_class_in_dir.av",
            new String[]{"指定是否跳过合并目录中的class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_other_in_dir.av",
            new String[]{"指定是否跳过合并目录中的非jar、war、class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT
            }, JavaCG2ElChecker4MergeFileInDir.class, true),
    ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_in_jar_war.av",
            new String[]{"指定是否跳过合并jar/war中的jar文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_class_in_jar_war.av",
            new String[]{"指定是否跳过合并jar/war中的class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_other_in_jar_war.av",
            new String[]{"指定是否跳过合并jar/war中的非jar、war、class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_war_by_class_dir_prefix.av",
            new String[]{"通过class文件目录的不同层级的路径前缀判断是否跳过合并当前jar/war文件",
                    "相当于通过jar/war文件中类的包名控制是否跳过合并当前jar/war文件",
                    "以下参数为jar/war文件中的class文件目录的不同层级的路径前缀集合",
                    "集合中的元素类型为字符串，以/作为分隔符，不会以分隔符开头或结尾",
                    "例如jar文件中有a1/c1.class、a2/b2/c2.class，则该jar文件中的class文件目录1级路径前缀有a1、a2，2级路径前缀有a2/b2，没有大于2级的路径前缀",
                    "在使用以下 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + " 参数时，需要以 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + " 开头，后续通过数字指定class文件路径层级",
                    "例如 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "3 代表第3级class文件路径集合",
                    "以下的 " + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + " " + JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName() + " 均代表需要判断是否需要跳过合并的jar/war文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR
            }, JavaCG2ElChecker4MergeFileInJarWar.class, true),
    ECE_PARSE_IGNORE_CLASS(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName() + "/parse_ignore_class.av",
            new String[]{"指定是否跳过解析类"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME
            }, JavaCG2ElChecker4ParseClass.class, true),
    ECE_PARSE_IGNORE_METHOD(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName() + "/parse_ignore_method.av",
            new String[]{"指定是否跳过解析方法"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME
            }, JavaCG2ElChecker4ParseMethod.class, true),
    ECE_PARSE_IGNORE_METHOD_CALL_ER(JavaCG2DirEnum.IDE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_er.av",
            new String[]{"指定是否跳过解析方法调用，只通过调用方法判断"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD,
            }, JavaCG2ElChecker4MethodCallEr.class, true),
    ECE_PARSE_IGNORE_METHOD_CALL_EE(JavaCG2DirEnum.IDE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_ee.av",
            new String[]{"指定是否跳过解析方法调用，只通过被调用方法判断"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD
            }, JavaCG2ElChecker4MethodCallEe.class, true),
    ECE_PARSE_IGNORE_METHOD_CALL_ER_EE(JavaCG2DirEnum.IDE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_er_ee.av",
            new String[]{"指定是否跳过解析方法调用，通过调用方法与被调用方法一起判断"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD
            }, JavaCG2ElChecker4MethodCallErEe.class, true),
    ;

    // 配置文件名称
    private final String fileName;

    // 配置文件描述
    private final String[] descriptions;

    // 配置文件允许使用的表达式变量枚举
    private final JavaCG2ElAllowedVariableEnum[] elAllowedVariableEnums;

    // 用于提前执行表达式进行检查的类
    private final Class<? extends ElChecker> elCheckClass;

    // 当前参数是否用于跳过数据
    private final boolean ignoreData;

    JavaCG2ElConfigEnum(String fileName, String[] descriptions, JavaCG2ElAllowedVariableEnum[] elAllowedVariableEnums, Class<? extends ElChecker> elCheckClass,
                        boolean ignoreData) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.elAllowedVariableEnums = elAllowedVariableEnums;
        this.elCheckClass = elCheckClass;
        this.ignoreData = ignoreData;
    }

    @Override
    public String getEnumConstantsName() {
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
    public Class<? extends ElChecker> getElCheckClass() {
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
