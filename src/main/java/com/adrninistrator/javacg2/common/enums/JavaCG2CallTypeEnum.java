package com.adrninistrator.javacg2.common.enums;

import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/8/2
 * @description: 方法调用类型枚举
 */

public enum JavaCG2CallTypeEnum {
    // 原始的调用类型，不以“_”开头，其他的都需要以“_”开头
    CTE_RAW_INVOKE_VIRTUAL("VIR", true, false, false, false, false,
            false, "JVM的原始方法指令: INVOKEVIRTUAL"),
    CTE_RAW_INVOKE_INTERFACE("INT", true, false, false, false, false,
            false, "JVM的原始方法指令: INVOKEINTERFACE"),
    CTE_RAW_INVOKE_SPECIAL("SPE", true, false, false, false, false,
            false, "JVM的原始方法指令: INVOKESPECIAL"),
    CTE_RAW_INVOKE_STATIC("STA", true, false, false, false, false,
            false, "JVM的原始方法指令: INVOKESTATIC"),
    CTE_RAW_INVOKE_DYNAMIC("DYN", true, false, false, false, false,
            false, "JVM的原始方法指令: INVOKEDYNAMIC"),
    // Spring Bean相关的调用类型
    CTE_SPRING_BEAN_ACTUAL_INTERFACE("_SPR_ACT_I", false, false, false, false, false,
            false, "被调用接口为 Spring Bean ，替换为实际的实现类类型"),
    CTE_SPRING_BEAN_ACTUAL_CLASS("_SPR_ACT_C", false, false, false, false, false,
            false, "被调用类为 Spring Bean ，替换为实际的子类类型"),
    // 其他调用类型
    CTE_ACTUAL_INTERFACE("_ACT_I", false, false, false, false, false,
            false, "被调用接口替换为实际的实现类类型"),
    CTE_ACTUAL_CLASS("_ACT_C", false, false, false, false, false,
            false, "被调用类替换为实际的子类类型"),
    CTE_INTERFACE_CALL_IMPL_CLASS("_ITF", false, false, true, false, false,
            false, "接口调用实现类对应的方法"),
    CTE_LAMBDA("_LM", false, false, false, false, false,
            false, "Lambda表达式"),
    CTE_RUNNABLE_INIT_RUN1("_RIR1", false, false, false, true, false,
            false, "其他方法调用Runnable 构造函数"),
    CTE_RUNNABLE_INIT_RUN2("_RIR2", false, false, false, true, true,
            false, "Runnable 构造函数调用 run() 方法"),
    CTE_CALLABLE_INIT_CALL1("_CIC1", false, false, false, true, false,
            false, "其他方法调用Callable 构造函数"),
    CTE_CALLABLE_INIT_CALL2("_CIC2", false, false, false, true, true,
            false, "Callable 构造函数调用 call() 方法"),
    CTE_TX_CALLBACK_INIT_CALL1("_TCID1", false, false, false, true, false,
            false, "其他方法调用 TransactionCallback 构造函数"),
    CTE_TX_CALLBACK_INIT_CALL2("_TCID2", false, false, false, true, false,
            true, "TransactionCallback 构造函数调用 doInTransaction() 方法"),
    CTE_TX_CALLBACK_WR_INIT_CALL1("_TCWRID1", false, false, false, true, false,
            false, "其他方法调用 TransactionCallbackWithoutResult 构造函数"),
    CTE_TX_CALLBACK_WR_INIT_CALL2("_TCWRID2", false, false, false, true, false,
            true, "TransactionCallbackWithoutResult 构造函数调用 doInTransactionWithoutResult() 方法"),
    CTE_THREAD_START_RUN("_TSR", false, false, false, false, true,
            false, "Thread start() 方法调用 run() 方法"),
    CTE_SUPER_CALL_CHILD("_SCC", false, false, true, false, false,
            false, "父类调用子类对应的方法"),
    CTE_CHILD_CALL_SUPER("_CCS", false, true, false, false, false,
            false, "子类调用父类对应的方法"),
    CTE_CHILD_CALL_SUPER_SPECIAL("_CCS_SPE", true, true, false, false, false,
            false, "子类通过super.调用父类方法"),
    CTE_CHILD_CALL_SUPER_INTERFACE("_CCS_I", false, false, false, false, false,
            false, "子接口调用父接口对应的方法"),
    CTE_CLASS_CALL_INTERFACE_DEFAULT("_CCID", false, true, false, false, false,
            false, "实现类调用接口对应的default方法"),
    CTE_INTERFACE_CALL_INTERFACE_DEFAULT("_ICID", false, true, false, false, false,
            false, "子接口调用父接口对应的default方法"),
    CTE_MANUAL_ADDED("_MA", false, false, false, false, false,
            false, "人工添加的方法调用"),
    CTE_METHOD_ANNOTATION_ADDED("_MAA", false, false, false, false, false,
            false, "通过方法注解添加的调用关系"),
    CTE_ILLEGAL("ILLEGAL", false, false, false, false, false,
            false, "ILLEGAL"),
    ;

    // 类型
    private final String type;
    // 存在实际指令的方法调用
    private final boolean existsInstruction;
    // 子类调用父类方法
    private final boolean childCallSuper;
    // 父类/接口调用子类/实现类方法
    private final boolean superCallChild;
    // <init>方法和其他方法的调用
    private final boolean initMethodCall;
    // 在其他线程中执行
    private final boolean runInOtherThread;
    // 在Spring事务中执行
    private final boolean runInSpringTx;
    // 描述
    private final String desc;

    // 存在实际指令的方法调用类型列表
    private static final List<String> EXISTS_INSTRUCTION_LIST;
    // 子类调用父类方法类型列表
    private static final List<String> CHILD_CALL_SUPER_LIST;
    // 父类/接口调用子类/实现类方法类型列表
    private static final List<String> SUPER_CALL_CHILD_LIST;
    // <init>方法和其他方法的调用类型列表
    private static final List<String> INIT_METHOD_CALL_LIST;
    // 在其他线程中执行类型列表
    private static final List<String> RUN_IN_OTHER_THREAD_LIST;
    // 在Spring事务中执行类型列表
    private static final List<String> RUN_IN_SPRING_TX_LIST;

    static {
        List<String> existsInstructionList = new ArrayList<>();
        List<String> childCallSuperList = new ArrayList<>();
        List<String> superCallChildList = new ArrayList<>();
        List<String> initMethodCallList = new ArrayList<>();
        List<String> runInOtherThreadList = new ArrayList<>();
        List<String> runInSpringTxList = new ArrayList<>();

        for (JavaCG2CallTypeEnum callTypeEnum : JavaCG2CallTypeEnum.values()) {
            if (callTypeEnum.isExistsInstruction()) {
                existsInstructionList.add(callTypeEnum.getType());
            }
            if (callTypeEnum.isChildCallSuper()) {
                childCallSuperList.add(callTypeEnum.getType());
            }
            if (callTypeEnum.isSuperCallChild()) {
                superCallChildList.add(callTypeEnum.getType());
            }
            if (callTypeEnum.isInitMethodCall()) {
                initMethodCallList.add(callTypeEnum.getType());
            }
            if (callTypeEnum.isRunInOtherThread()) {
                runInOtherThreadList.add(callTypeEnum.getType());
            }
            if (callTypeEnum.isRunInSpringTx()) {
                runInSpringTxList.add(callTypeEnum.getType());
            }
        }

        EXISTS_INSTRUCTION_LIST = Collections.unmodifiableList(existsInstructionList);
        CHILD_CALL_SUPER_LIST = Collections.unmodifiableList(childCallSuperList);
        SUPER_CALL_CHILD_LIST = Collections.unmodifiableList(superCallChildList);
        INIT_METHOD_CALL_LIST = Collections.unmodifiableList(initMethodCallList);
        RUN_IN_OTHER_THREAD_LIST = Collections.unmodifiableList(runInOtherThreadList);
        RUN_IN_SPRING_TX_LIST = Collections.unmodifiableList(runInSpringTxList);
    }

    JavaCG2CallTypeEnum(String type, boolean existsInstruction, boolean childCallSuper, boolean superCallChild, boolean initMethodCall, boolean runInOtherThread,
                        boolean runInSpringTx,
                        String desc) {
        this.type = type;
        this.existsInstruction = existsInstruction;
        this.childCallSuper = childCallSuper;
        this.superCallChild = superCallChild;
        this.initMethodCall = initMethodCall;
        this.runInOtherThread = runInOtherThread;
        this.runInSpringTx = runInSpringTx;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public boolean isExistsInstruction() {
        return existsInstruction;
    }

    public boolean isChildCallSuper() {
        return childCallSuper;
    }

    public boolean isSuperCallChild() {
        return superCallChild;
    }

    public boolean isInitMethodCall() {
        return initMethodCall;
    }

    public boolean isRunInOtherThread() {
        return runInOtherThread;
    }

    public boolean isRunInSpringTx() {
        return runInSpringTx;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return type + "-" + desc;
    }

    public static JavaCG2CallTypeEnum getFromType(String type) {
        for (JavaCG2CallTypeEnum callTypeEnum : JavaCG2CallTypeEnum.values()) {
            if (callTypeEnum.getType().equals(type)) {
                return callTypeEnum;
            }
        }
        return null;
    }

    /**
     * 检查方法调用枚举类型是否重复定义
     */
    public static void checkRepeat() {
        Map<String, List<String>> enumInfoMap = new HashMap<>();

        for (JavaCG2CallTypeEnum callTypeEnum : JavaCG2CallTypeEnum.values()) {
            List<String> enumNameList = enumInfoMap.computeIfAbsent(callTypeEnum.getType(), k -> new ArrayList<>());
            enumNameList.add(callTypeEnum.name());
            if (enumNameList.size() > 1) {
                String repeatedEnumTypes = StringUtils.join(enumNameList, " ");
                throw new JavaCG2RuntimeException(JavaCG2CallTypeEnum.class.getSimpleName() + " 类定义的枚举类型存在重复" + callTypeEnum.getType() + " " + repeatedEnumTypes);
            }
        }
    }

    public static List<String> getExistsInstructionList() {
        return EXISTS_INSTRUCTION_LIST;
    }

    public static List<String> getChildCallSuperList() {
        return CHILD_CALL_SUPER_LIST;
    }

    public static List<String> getSuperCallChildList() {
        return SUPER_CALL_CHILD_LIST;
    }

    public static List<String> getInitMethodCallList() {
        return INIT_METHOD_CALL_LIST;
    }

    public static List<String> getRunInOtherThreadList() {
        return RUN_IN_OTHER_THREAD_LIST;
    }

    public static List<String> getRunInSpringTxList() {
        return RUN_IN_SPRING_TX_LIST;
    }

    /**
     * 判断是否属于子类调用父类方法类型
     *
     * @param callType
     * @return
     */
    public static boolean isChildCallSuperType(String callType) {
        return callType != null && CHILD_CALL_SUPER_LIST.contains(callType);
    }

    /**
     * 判断是否属于父类/接口调用子类/实现类方法类型
     *
     * @param callType
     * @return
     */
    public static boolean isSuperCallChildType(String callType) {
        return callType != null && SUPER_CALL_CHILD_LIST.contains(callType);
    }

    /**
     * 判断是否属于<init>方法和其他方法的调用类型
     *
     * @param callType
     * @return
     */
    public static boolean isInitMethodCallType(String callType) {
        return callType != null && INIT_METHOD_CALL_LIST.contains(callType);
    }

    /**
     * 判断是否属于在其他线程中执行类型
     *
     * @param callType
     * @return
     */
    public static boolean isRunInOtherThreadType(String callType) {
        return callType != null && RUN_IN_OTHER_THREAD_LIST.contains(callType);
    }

    /**
     * 判断是否属于在Spring事务中执行类型
     *
     * @param callType
     * @return
     */
    public static boolean isRunInSpringTxType(String callType) {
        return callType != null && RUN_IN_SPRING_TX_LIST.contains(callType);
    }
}
