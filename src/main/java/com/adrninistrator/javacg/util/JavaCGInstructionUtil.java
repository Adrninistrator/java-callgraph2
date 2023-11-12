package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.bcel.Const;
import org.apache.bcel.generic.BranchInstruction;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.INVOKEDYNAMIC;
import org.apache.bcel.generic.IndexedInstruction;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.InvokeInstruction;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;

/**
 * @author adrninistrator
 * @date 2022/10/19
 * @description:
 */
public class JavaCGInstructionUtil {

    /**
     * 判断是否为方法调用类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isMethodInvokeInstruction(short opCode) {
        return opCode >= Const.INVOKEVIRTUAL && opCode <= Const.INVOKEDYNAMIC;
    }

    /**
     * 判断是否为方法调用类指令，排除INVOKEDYNAMIC
     *
     * @param opCode
     * @return
     */
    public static boolean isMethodInvokeInstructionExcludeDynamic(short opCode) {
        return opCode >= Const.INVOKEVIRTUAL && opCode <= Const.INVOKEINTERFACE;
    }

    /**
     * 判断是否为if类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isIfInstruction(int opCode) {
        return (opCode >= Const.IFEQ && opCode <= Const.IF_ACMPNE) ||
                opCode == Const.IFNULL ||
                opCode == Const.IFNONNULL;
    }

    /**
     * 判断是否为switch类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isSwitchInstruction(int opCode) {
        return opCode == Const.TABLESWITCH || opCode == Const.LOOKUPSWITCH;
    }

    /**
     * 判断是否为goto类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isGotoInstruction(int opCode) {
        return opCode == Const.GOTO || opCode == Const.GOTO_W;
    }

    /**
     * 判断是否为return或throw类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isExitInstruction(int opCode) {
        return (opCode >= Const.IRETURN && opCode <= Const.RETURN) ||
                opCode == Const.ATHROW;
    }

    /**
     * 判断是否为jsr类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isJsrInstruction(int opCode) {
        return opCode == Const.JSR || opCode == Const.JSR_W;
    }

    /**
     * 判断是否为ret指令
     *
     * @param opCode
     * @return
     */
    public static boolean isRetInstruction(int opCode) {
        return opCode == Const.RET;
    }

    /**
     * 判断是否为带返回值的return类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isReturnWithValueInstruction(int opCode) {
        return opCode >= Const.IRETURN && opCode <= Const.ARETURN;
    }

    /**
     * 获取指令用于打印的信息
     *
     * @param ih
     * @return
     */
    public static String getInstructionHandlePrintInfo(InstructionHandle ih) {
        Instruction instruction = ih.getInstruction();
        String extra = "";
        if (instruction instanceof IndexedInstruction) {
            extra = " " + ((IndexedInstruction) instruction).getIndex();
        } else if (instruction instanceof BranchInstruction) {
            extra = " " + ((BranchInstruction) instruction).getTarget().getPosition();
        }
        return "[" + ih.getPosition() + "] " + instruction.getClass().getSimpleName() + extra;
    }

    /**
     * 获取方法第1个InstructionHandle
     *
     * @param mg
     * @return
     */
    public static InstructionHandle getFirstInstructionHandle(MethodGen mg) {
        InstructionList instructionList = mg.getInstructionList();
        if (instructionList == null || instructionList.isEmpty()) {
            return null;
        }

        return instructionList.getStart();
    }

    /**
     * 根据方法调用指令获取被调用方法信息（不处理INVOKEDYNAMIC指令）
     *
     * @param invokeInstruction
     * @param cpg
     * @return
     */
    public static JavaCGMethodInfo getCalleeMethodInfo(InvokeInstruction invokeInstruction, ConstantPoolGen cpg) {
        String calleeMethodName = invokeInstruction.getMethodName(cpg);
        Type[] calleeArgTypes = invokeInstruction.getArgumentTypes(cpg);
        if (invokeInstruction instanceof INVOKEDYNAMIC) {
            // getReferenceType()方法获取到的类型为java.lang.Object
            String calleeClassName = invokeInstruction.getType(cpg).toString();
            return new JavaCGMethodInfo(calleeClassName, calleeMethodName, calleeArgTypes, invokeInstruction.getReturnType(cpg));
        }

        String calleeClassName = invokeInstruction.getReferenceType(cpg).toString();
        return new JavaCGMethodInfo(calleeClassName, calleeMethodName, calleeArgTypes, invokeInstruction.getReturnType(cpg));
    }

    private JavaCGInstructionUtil() {
        throw new IllegalStateException("illegal");
    }
}
