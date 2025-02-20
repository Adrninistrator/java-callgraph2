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
    ECE_EXAMPLE("el_example" + JavaCG2Constants.EXT_MD, new String[]{"表达式示例文件"}, null, null),
    ECE_MERGE_FILE_IGNORE_JAR_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_in_dir.av",
            new String[]{"目录中的jar文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class),
    ECE_MERGE_FILE_IGNORE_WAR_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_war_in_dir.av",
            new String[]{"目录中的war文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class),
    ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_class_in_dir.av",
            new String[]{"目录中的class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInDir.class),
    ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_other_in_dir.av",
            new String[]{"目录中的非jar、war、class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT_LOWER
            }, JavaCG2ElChecker4MergeFileInDir.class),
    ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_jar_in_jar_war.av",
            new String[]{"jar/war中的jar文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME
            }, JavaCG2ElChecker4MergeFileInJarWar.class),
    ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_class_in_jar_war.av",
            new String[]{"jar/war中的class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR
            }, JavaCG2ElChecker4MergeFileInJarWar.class),
    ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName() + "/ignore_other_in_jar_war.av",
            new String[]{"jar/war中的非jar、war、class文件"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT_LOWER
            }, JavaCG2ElChecker4MergeFileInJarWar.class),
    ECE_PARSE_IGNORE_CLASS(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName() + "/parse_ignore_class.av",
            new String[]{"类"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME
            }, JavaCG2ElChecker4ParseClass.class),
    ECE_PARSE_IGNORE_METHOD(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName() + "/parse_ignore_method.av",
            new String[]{"方法"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME
            }, JavaCG2ElChecker4ParseMethod.class),
    ECE_PARSE_IGNORE_METHOD_CALL_ER(JavaCG2DirEnum.IDE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_er.av",
            new String[]{"方法调用，只通过调用方法判断"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD,
            }, JavaCG2ElChecker4MethodCallEr.class),
    ECE_PARSE_IGNORE_METHOD_CALL_EE(JavaCG2DirEnum.IDE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_ee.av",
            new String[]{"方法调用，只通过被调用方法判断"},
            new JavaCG2ElAllowedVariableEnum[]{
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM,
                    JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD
            }, JavaCG2ElChecker4MethodCallEe.class),
    ECE_PARSE_IGNORE_METHOD_CALL_ER_EE(JavaCG2DirEnum.IDE_METHOD_CALL_SWITCH.getDirName() + "/parse_ignore_method_call_er_ee.av",
            new String[]{"方法调用，通过调用方法与被调用方法一起判断"},
            new JavaCG2ElAllowedVariableEnum[]{
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
            }, JavaCG2ElChecker4MethodCallErEe.class),
    ;

    // 配置文件名称
    private final String fileName;

    // 配置文件描述
    private final String[] descriptions;

    // 配置文件允许使用的表达式变量枚举
    private final JavaCG2ElAllowedVariableEnum[] elAllowedVariableEnums;

    // 用于提前执行表达式进行检查的类
    private final Class<? extends ElChecker> elCheckClass;

    JavaCG2ElConfigEnum(String fileName, String[] descriptions, JavaCG2ElAllowedVariableEnum[] elAllowedVariableEnums, Class<? extends ElChecker> elCheckClass) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.elAllowedVariableEnums = elAllowedVariableEnums;
        this.elCheckClass = elCheckClass;
    }

    @Override
    public String getEnumName() {
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
    public String getConfigPrintInfo() {
        return fileName + " " + JavaCG2ElConfigEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public ElAllowedVariableInterface[] getElAllowedVariableEnums() {
        return elAllowedVariableEnums;
    }
}
