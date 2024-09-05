package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.dto.method.JavaCG2MethodInfo;
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
import org.apache.bcel.generic.TypedInstruction;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/10/19
 * @description:
 */
public class JavaCG2InstructionUtil {

    // goto及return类指令的opcode
    public static final short[] GOTO_RETURN_OPCODES;

    static {
        List<Short> gotoReturnOpcodeList = new ArrayList<>();
        gotoReturnOpcodeList.add(Const.GOTO);
        gotoReturnOpcodeList.add(Const.GOTO_W);
        for (short opCode = Const.IRETURN; opCode <= Const.RETURN; opCode++) {
            gotoReturnOpcodeList.add(opCode);
        }
        GOTO_RETURN_OPCODES = new short[gotoReturnOpcodeList.size()];
        for (int i = 0; i < gotoReturnOpcodeList.size(); i++) {
            GOTO_RETURN_OPCODES[i] = gotoReturnOpcodeList.get(i);
        }
    }

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
     * 判断是否为return类指令
     *
     * @param opCode
     * @return
     */
    public static boolean isReturnInstruction(int opCode) {
        return opCode >= Const.IRETURN && opCode <= Const.RETURN;
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
     * 获取方法第一个InstructionHandle
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
    public static JavaCG2MethodInfo getCalleeMethodInfo(InvokeInstruction invokeInstruction, ConstantPoolGen cpg) {
        String calleeMethodName = invokeInstruction.getMethodName(cpg);
        Type[] calleeArgTypes = invokeInstruction.getArgumentTypes(cpg);
        if (invokeInstruction instanceof INVOKEDYNAMIC) {
            // getReferenceType()方法获取到的类型为java.lang.Object
            String calleeClassName = JavaCG2InstructionUtil.getTypeString(invokeInstruction, cpg);
            return new JavaCG2MethodInfo(calleeClassName, calleeMethodName, calleeArgTypes, invokeInstruction.getReturnType(cpg));
        }

        String calleeClassName = invokeInstruction.getReferenceType(cpg).toString();
        return new JavaCG2MethodInfo(calleeClassName, calleeMethodName, calleeArgTypes, invokeInstruction.getReturnType(cpg));
    }

    /**
     * 从指定的指令开始往后找，找到第一个为指定类型的指令，到指定的结束指令为止
     *
     * @param startPosition
     * @param endPosition
     * @param opCodes
     * @return
     */
    public static InstructionHandle findFirstIhBetween(InstructionList instructionList, int startPosition, int endPosition, short... opCodes) {
        InstructionHandle startIh = instructionList.findHandle(startPosition);
        if (startIh == null) {
            return null;
        }
        InstructionHandle currentIh = startIh;
        do {
            short currentOpCode = currentIh.getInstruction().getOpcode();
            for (short opCode : opCodes) {
                if (opCode == currentOpCode) {
                    return currentIh;
                }
            }
            currentIh = currentIh.getNext();
        } while (currentIh != null && currentIh.getPosition() <= endPosition);
        return null;
    }

    /**
     * 从指定的指令开始往前找，找到第一个为指定类型的指令
     *
     * @param ih
     * @param opCodes
     * @return
     */
    public static InstructionHandle findLastIh(InstructionHandle ih, short... opCodes) {
        InstructionHandle currentIh = ih;
        do {
            short currentOpCode = currentIh.getInstruction().getOpcode();
            for (short opCode : opCodes) {
                if (opCode == currentOpCode) {
                    return currentIh;
                }
            }
            currentIh = currentIh.getPrev();
        } while (currentIh != null);
        return null;
    }

    /**
     * 获取指定偏移量对应的指定的前一个指令的偏移量
     *
     * @param instructionList
     * @param position
     * @return
     */
    public static int getInstructionPositionBefore(InstructionList instructionList, int position) {
        InstructionHandle instructionHandle = instructionList.findHandle(position).getPrev();
        return instructionHandle.getPosition();
    }

    /**
     * 获取指令对应的类型字符串
     *
     * @param typedInstruction
     * @param cpg
     * @return
     */
    public static String getTypeString(TypedInstruction typedInstruction, ConstantPoolGen cpg) {
        return typedInstruction.getType(cpg).toString();
    }

    private JavaCG2InstructionUtil() {
        throw new IllegalStateException("illegal");
    }
}
