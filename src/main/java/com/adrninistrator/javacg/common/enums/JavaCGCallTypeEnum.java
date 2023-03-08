package com.adrninistrator.javacg.common.enums;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/8/2
 * @description: 方法调用类型枚举
 */

public enum JavaCGCallTypeEnum {
    // 原始的调用类型，不以“_”开头，其他的都需要以“_”开头
    CTE_RAW_INVOKE_VIRTUAL("VIR", "INVOKEVIRTUAL"),
    CTE_RAW_INVOKE_INTERFACE("INT", "INVOKEINTERFACE"),
    CTE_RAW_INVOKE_SPECIAL("SPE", "INVOKESPECIAL"),
    CTE_RAW_INVOKE_STATIC("STA", "INVOKESTATIC"),
    CTE_RAW_INVOKE_DYNAMIC("DYN", "INVOKEDYNAMIC"),
    // Spring Bean相关的调用类型
    CTE_SPRING_BEAN_ACTUAL_INTERFACE("_SPR_ACT_I", "被调用接口为 Spring Bean ，替换为实际的类型"),
    CTE_SPRING_BEAN_ACTUAL_CLASS("_SPR_ACT_C", "被调用类为 Spring Bean ，替换为实际的类型"),
    // 其他调用类型
    CTE_ACTUAL_INTERFACE("_ACT_I", "被调用接口替换为实际的类型"),
    CTE_ACTUAL_CLASS("_ACT_C", "被调用类替换为实际的类型"),
    CTE_INTERFACE_CALL_IMPL_CLASS("_ITF", "接口调用实现类"),
    CTE_LAMBDA("_LM", "Lambda表达式"),
    CTE_RUNNABLE_INIT_RUN1("_RIR1", "其他方法调用Runnable 构造函数"),
    CTE_RUNNABLE_INIT_RUN2("_RIR2", "Runnable 构造函数调用 run() 方法"),
    CTE_CALLABLE_INIT_CALL1("_CIC1", "其他方法调用Callable 构造函数"),
    CTE_CALLABLE_INIT_CALL2("_CIC2", "Callable 构造函数调用 call() 方法"),
    CTE_TX_CALLBACK_INIT_CALL1("_TCID1", "其他方法调用TransactionCallback 构造函数"),
    CTE_TX_CALLBACK_INIT_CALL2("_TCID2", "TransactionCallback 构造函数调用 doInTransaction() 方法"),
    CTE_TX_CALLBACK_WR_INIT_CALL1("_TCWRID1", "其他方法调用TransactionCallbackWithoutResult 构造函数"),
    CTE_TX_CALLBACK_WR_INIT_CALL2("_TCWRID2", "TransactionCallbackWithoutResult 构造函数调用 doInTransactionWithoutResult() 方法"),
    CTE_THREAD_START_RUN("_TSR", "Thread start() 方法调用 run() 方法"),
    CTE_SUPER_CALL_CHILD("_SCC", "父类调用子类方法"),
    CTE_CHILD_CALL_SUPER("_CCS", "子类调用父类方法"),
    CTE_CHILD_CALL_SUPER_INTERFACE("_CCS_I", "子接口调用父接口方法"),
    CTE_MANUAL_ADDED("_MA", "人工添加的方法调用"),
    CTE_METHOD_ANNOTATION_ADDED("_MAA", "通过方法注解添加的调用关系"),
    CTE_ILLEGAL("ILLEGAL", "ILLEGAL"),
    ;

    private final String type;

    private final String desc;

    JavaCGCallTypeEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }

    public static JavaCGCallTypeEnum getFromType(String type) {
        for (JavaCGCallTypeEnum callTypeEnum : JavaCGCallTypeEnum.values()) {
            if (callTypeEnum.getType().equals(type)) {
                return callTypeEnum;
            }
        }
        return JavaCGCallTypeEnum.CTE_ILLEGAL;
    }

    /**
     * 检查方法调用枚举类型是否重复定义
     */
    public static void checkRepeat() {
        Map<String, List<String>> enumInfoMap = new HashMap<>();

        for (JavaCGCallTypeEnum callTypeEnum : JavaCGCallTypeEnum.values()) {
            List<String> enumNameList = enumInfoMap.computeIfAbsent(callTypeEnum.getType(), k -> new ArrayList<>());
            enumNameList.add(callTypeEnum.name());
            if (enumNameList.size() > 1) {
                String repeatedEnumTypes = StringUtils.join(enumNameList, " ");
                throw new JavaCGRuntimeException(JavaCGCallTypeEnum.class.getSimpleName() + " 类定义的枚举类型存在重复" + callTypeEnum.getType() + " " + repeatedEnumTypes);
            }
        }
    }

    @Override
    public String toString() {
        return type + "-" + desc;
    }
}
