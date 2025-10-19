package com.adrninistrator.javacg2.el.constants;

import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/10/3
 * @description:
 */
public class JavaCG2ElConstants {

    private static final Map<String, String[]> EL_DIR_USAGE_MAP = new HashMap<>();

    static {
        EL_DIR_USAGE_MAP.put(JavaCG2DirEnum.IDE_MERGE_FILE_SWITCH.getDirName(),
                new String[]{"在 合并需要解析的目录或jar/war文件时 使用的表达式",
                        "若当前配置文件中的表达式执行结果为 true，则跳过合并（及解析）对应的文件",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的文件"});
        EL_DIR_USAGE_MAP.put(JavaCG2DirEnum.IDE_PARSE_CLASS_METHOD_SWITCH.getDirName(),
                new String[]{"在 解析类或方法时 使用的表达式",
                        "若当前配置文件中的表达式执行结果为 true，则跳过解析对应的类或方法",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的类或方法"});
        EL_DIR_USAGE_MAP.put(JavaCG2DirEnum.IDE_PARSE_METHOD_CALL_SWITCH.getDirName(),
                new String[]{"在 解析方法调用时 使用的表达式",
                        "若当前配置文件中的表达式执行结果为 true，则跳过解析对应的方法调用",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法调用"});
        EL_DIR_USAGE_MAP.put(JavaCG2DirEnum.IDE_HANDLE_XML_SWITCH.getDirName(),
                new String[]{"在 处理XML文件时 使用的表达式",
                        "若当前配置文件中的表达式执行结果为 true，则跳过对应的XML文件中的元素",
                        "若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的XML文件中的元素"});
    }

    public static Map<String, String[]> getElDirUsageMap() {
        return EL_DIR_USAGE_MAP;
    }

    private JavaCG2ElConstants() {
        throw new IllegalStateException("illegal");
    }
}
