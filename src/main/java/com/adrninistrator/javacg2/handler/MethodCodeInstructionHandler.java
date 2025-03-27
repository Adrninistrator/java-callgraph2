package com.adrninistrator.javacg2.handler;

import com.adrninistrator.javacg2.dto.methodcode.MethodCodeInstruction;
import com.adrninistrator.javacg2.dto.methodcode.MethodCodeInstructionArg;
import com.adrninistrator.javacg2.dto.methodcode.MethodCodeInstructionJump;
import com.adrninistrator.javacg2.dto.methodcode.MethodCodeInstructionLookupSwitch;
import com.adrninistrator.javacg2.dto.methodcode.MethodCodeInstructionLookupSwitchJump;
import com.adrninistrator.javacg2.dto.methodcode.MethodCodeInstructionTableSwitch;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.ClassFormatException;
import org.apache.bcel.classfile.Code;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.util.ByteSequence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/3/20
 * @description: 处理方法代码指令，生成格式化后的字符串
 */
public class MethodCodeInstructionHandler {

    private static final Logger logger = LoggerFactory.getLogger(MethodCodeInstructionHandler.class);

    /*
     * The 'WIDE' instruction is used in the byte code to allow 16-bit wide indices for local variables. This opcode
     * precedes an 'ILOAD', e.g.. The opcode immediately following takes an extra byte which is combined with the following
     * byte to form a 16-bit value.
     */
    private static boolean wide;

    /**
     * 根据以下方法修改
     * org.apache.bcel.classfile.Utility#codeToString(byte[], org.apache.bcel.classfile.ConstantPool, int, int, boolean)
     *
     * @param method
     * @return
     */
    public static String getMethodCodeStr(Method method) {
        Code code = method.getCode();
        if (code == null) {
            return "";
        }
        return codeToString(code.getCode(), method.getConstantPool());
    }

    private static String genCodeString(List<MethodCodeInstruction> methodCodeInstructionList, Map<Integer, Integer> instructionOffsetSeqMap) {
        StringBuilder stringBuilder = new StringBuilder();
        for (MethodCodeInstruction methodCodeInstruction : methodCodeInstructionList) {
            int seq = instructionOffsetSeqMap.get(methodCodeInstruction.getOffset());
            stringBuilder.append(seq).append("\t").append(methodCodeInstruction.getInstructionName());
            if (methodCodeInstruction instanceof MethodCodeInstructionArg) {
                MethodCodeInstructionArg methodCodeInstructionArg = (MethodCodeInstructionArg) methodCodeInstruction;
                stringBuilder.append("\t").append(methodCodeInstructionArg.getArg());
            } else if (methodCodeInstruction instanceof MethodCodeInstructionJump) {
                MethodCodeInstructionJump methodCodeInstructionJump = (MethodCodeInstructionJump) methodCodeInstruction;
                int jumpSeq = instructionOffsetSeqMap.get(methodCodeInstructionJump.getTargetOffset());
                stringBuilder.append("\t").append(jumpSeq);
            } else if (methodCodeInstruction instanceof MethodCodeInstructionLookupSwitch) {
                MethodCodeInstructionLookupSwitch methodCodeInstructionLookupSwitch = (MethodCodeInstructionLookupSwitch) methodCodeInstruction;
                int defaultJumpSeq = instructionOffsetSeqMap.get(methodCodeInstructionLookupSwitch.getDefaultTargetOffset());
                stringBuilder.append("\t").append("(").append(defaultJumpSeq).append(")")
                        .append("\t").append("(").append(methodCodeInstructionLookupSwitch.getPairsNum()).append(")")
                        .append("\t{");
                List<MethodCodeInstructionLookupSwitchJump> jumpList = methodCodeInstructionLookupSwitch.getJumpList();
                for (MethodCodeInstructionLookupSwitchJump lookupSwitchJump : jumpList) {
                    int jumpSeq = instructionOffsetSeqMap.get(lookupSwitchJump.getTargetOffset());
                    stringBuilder.append("(").append(lookupSwitchJump.getIndex()).append(",").append(jumpSeq).append(")");
                }
                stringBuilder.append("}");
            } else if (methodCodeInstruction instanceof MethodCodeInstructionTableSwitch) {
                MethodCodeInstructionTableSwitch codeInstructionTableSwitch = (MethodCodeInstructionTableSwitch) methodCodeInstruction;
                int defaultJumpSeq = instructionOffsetSeqMap.get(codeInstructionTableSwitch.getDefaultTargetOffset());
                stringBuilder.append("\t").append("(").append(defaultJumpSeq).append(")")
                        .append("\t").append("(").append(codeInstructionTableSwitch.getLow()).append(",").append(codeInstructionTableSwitch.getHigh()).append(")")
                        .append("\t{");
                List<Integer> jumpTargetOffsetList = codeInstructionTableSwitch.getJumpTargetOffsetList();
                for (Integer jumpTargetOffset : jumpTargetOffsetList) {
                    int jumpSeq = instructionOffsetSeqMap.get(jumpTargetOffset);
                    stringBuilder.append("(").append(jumpSeq).append(")");
                }
            }
            stringBuilder.append("\n");
        }
        return stringBuilder.toString();
    }

    private static String codeToString(final byte[] code, final ConstantPool constantPool) {
        List<MethodCodeInstruction> methodCodeInstructionList = new ArrayList<>();
        Map<Integer, Integer> instructionOffsetSeqMap = new HashMap<>();
        int seq = -1;
        try (ByteSequence stream = new ByteSequence(code)) {
            while (stream.available() > 0) {
                int offset = stream.getIndex();
                instructionOffsetSeqMap.put(offset, ++seq);
                MethodCodeInstruction methodCodeInstruction = codeToString(stream, constantPool);
                methodCodeInstruction.setOffset(offset);
                methodCodeInstructionList.add(methodCodeInstruction);
            }
            return genCodeString(methodCodeInstructionList, instructionOffsetSeqMap);
        } catch (final IOException e) {
            throw new ClassFormatException("Byte code error: ", e);
        }
    }

    private static MethodCodeInstruction codeToString(final ByteSequence bytes, final ConstantPool constantPool) throws IOException {
        MethodCodeInstruction methodCodeInstruction = null;
        final short opcode = (short) bytes.readUnsignedByte();
        int defaultOffset = 0;
        int low;
        int high;
        int npairs;
        int index;
        int vindex;
        int constant;
        int[] match;
        int[] jumpTable;
        int noPadBytes = 0;
        int offset;
        String instructionName = Const.getOpcodeName(opcode);
        /*
         * Special case: Skip (0-3) padding bytes, i.e., the following bytes are 4-byte-aligned
         */
        if (opcode == Const.TABLESWITCH || opcode == Const.LOOKUPSWITCH) {
            final int remainder = bytes.getIndex() % 4;
            noPadBytes = remainder == 0 ? 0 : 4 - remainder;
            for (int i = 0; i < noPadBytes; i++) {
                byte b;
                if ((b = bytes.readByte()) != 0) {
                    logger.warn("Warning: Padding byte != 0 in {}", Const.getOpcodeName(opcode) + ":" + b);
                }
            }
            // Both cases have a field default_offset in common
            defaultOffset = bytes.readInt();
        }
        switch (opcode) {
            /*
             * Table switch has variable length arguments.
             */
            case Const.TABLESWITCH:
                methodCodeInstruction = new MethodCodeInstructionTableSwitch();
                low = bytes.readInt();
                high = bytes.readInt();
                offset = bytes.getIndex() - 12 - noPadBytes - 1;
                defaultOffset += offset;
                List<Integer> jumpTargetOffsetList = new ArrayList<>(high - low + 1);
                MethodCodeInstructionTableSwitch methodCodeInstructionTableSwitch = (MethodCodeInstructionTableSwitch) methodCodeInstruction;
                methodCodeInstructionTableSwitch.setDefaultTargetOffset(defaultOffset);
                methodCodeInstructionTableSwitch.setLow(low);
                methodCodeInstructionTableSwitch.setHigh(high);
                methodCodeInstructionTableSwitch.setJumpTargetOffsetList(jumpTargetOffsetList);
                jumpTable = new int[high - low + 1];
                for (int i = 0; i < jumpTable.length; i++) {
                    jumpTable[i] = offset + bytes.readInt();
                    jumpTargetOffsetList.add(jumpTable[i]);
                }
                break;
            /*
             * Lookup switch has variable length arguments.
             */
            case Const.LOOKUPSWITCH: {
                methodCodeInstruction = new MethodCodeInstructionLookupSwitch();
                npairs = bytes.readInt();
                offset = bytes.getIndex() - 8 - noPadBytes - 1;
                match = new int[npairs];
                jumpTable = new int[npairs];
                defaultOffset += offset;
                List<MethodCodeInstructionLookupSwitchJump> jumpList = new ArrayList<>(npairs);
                MethodCodeInstructionLookupSwitch methodCodeInstructionLookupSwitch = (MethodCodeInstructionLookupSwitch) methodCodeInstruction;
                methodCodeInstructionLookupSwitch.setDefaultTargetOffset(defaultOffset);
                methodCodeInstructionLookupSwitch.setPairsNum(npairs);
                methodCodeInstructionLookupSwitch.setJumpList(jumpList);
                for (int i = 0; i < npairs; i++) {
                    match[i] = bytes.readInt();
                    jumpTable[i] = offset + bytes.readInt();
                    MethodCodeInstructionLookupSwitchJump methodCodeInstructionLookupSwitchJump = new MethodCodeInstructionLookupSwitchJump();
                    methodCodeInstructionLookupSwitchJump.setIndex(match[i]);
                    methodCodeInstructionLookupSwitchJump.setIndex(jumpTable[i]);
                    jumpList.add(methodCodeInstructionLookupSwitchJump);
                }
            }
            break;
            /*
             * Two address bytes + offset from start of byte stream form the jump target
             */
            case Const.GOTO:
            case Const.IFEQ:
            case Const.IFGE:
            case Const.IFGT:
            case Const.IFLE:
            case Const.IFLT:
            case Const.JSR:
            case Const.IFNE:
            case Const.IFNONNULL:
            case Const.IFNULL:
            case Const.IF_ACMPEQ:
            case Const.IF_ACMPNE:
            case Const.IF_ICMPEQ:
            case Const.IF_ICMPGE:
            case Const.IF_ICMPGT:
            case Const.IF_ICMPLE:
            case Const.IF_ICMPLT:
            case Const.IF_ICMPNE:
                methodCodeInstruction = new MethodCodeInstructionJump();
                ((MethodCodeInstructionJump) methodCodeInstruction).setTargetOffset(bytes.getIndex() - 1 + bytes.readShort());
                break;
            /*
             * 32-bit wide jumps
             */
            case Const.GOTO_W:
            case Const.JSR_W:
                methodCodeInstruction = new MethodCodeInstructionJump();
                ((MethodCodeInstructionJump) methodCodeInstruction).setTargetOffset(bytes.getIndex() - 1 + bytes.readInt());
                break;
            /*
             * Index byte references local variable (register)
             */
            case Const.ALOAD:
            case Const.ASTORE:
            case Const.DLOAD:
            case Const.DSTORE:
            case Const.FLOAD:
            case Const.FSTORE:
            case Const.ILOAD:
            case Const.ISTORE:
            case Const.LLOAD:
            case Const.LSTORE:
            case Const.RET:
                if (wide) {
                    vindex = bytes.readUnsignedShort();
                    wide = false; // Clear flag
                } else {
                    vindex = bytes.readUnsignedByte();
                }
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(String.valueOf(vindex));
                break;
            /*
             * Remember wide byte which is used to form a 16-bit address in the following instruction. Relies on that the method is
             * called again with the following opcode.
             */
            case Const.WIDE:
                wide = true;
                break;
            /*
             * Array of basic type.
             */
            case Const.NEWARRAY:
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(Const.getTypeName(bytes.readByte()));
                break;
            /*
             * Access object/class fields.
             */
            case Const.GETFIELD:
            case Const.GETSTATIC:
            case Const.PUTFIELD:
            case Const.PUTSTATIC:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, Const.CONSTANT_Fieldref));
                break;
            /*
             * Operands are references to classes in constant pool
             */
            case Const.NEW:
            case Const.CHECKCAST:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, Const.CONSTANT_Class));
                break;
            case Const.INSTANCEOF:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, Const.CONSTANT_Class));
                break;
            /*
             * Operands are references to methods in constant pool
             */
            case Const.INVOKESPECIAL:
            case Const.INVOKESTATIC:
                index = bytes.readUnsignedShort();
                final Constant c = constantPool.getConstant(index);
                // With Java8 operand may be either a CONSTANT_Methodref
                // or a CONSTANT_InterfaceMethodref. (markro)
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, c.getTag()));
                break;
            case Const.INVOKEVIRTUAL:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, Const.CONSTANT_Methodref));
                break;
            case Const.INVOKEINTERFACE:
                index = bytes.readUnsignedShort();
                bytes.readUnsignedByte(); // historical, redundant
                bytes.readUnsignedByte(); // Last byte is a reserved space
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, Const.CONSTANT_InterfaceMethodref));
                break;
            case Const.INVOKEDYNAMIC:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, Const.CONSTANT_InvokeDynamic));
                bytes.readUnsignedByte(); // Thrid byte is a reserved space
                bytes.readUnsignedByte(); // Last byte is a reserved space
                break;
            /*
             * Operands are references to items in constant pool
             */
            case Const.LDC_W:
            case Const.LDC2_W:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                methodCodeInstruction.setInstructionName("LDC");
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, constantPool.getConstant(index).getTag()));
                break;
            case Const.LDC:
                index = bytes.readUnsignedByte();
                methodCodeInstruction = new MethodCodeInstructionArg();
                methodCodeInstruction.setInstructionName("LDC");
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(constantPool.constantToString(index, constantPool.getConstant(index).getTag()));
                break;
            /*
             * Array of references.
             */
            case Const.ANEWARRAY:
                index = bytes.readUnsignedShort();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(Utility.compactClassName(constantPool.getConstantString(index, Const.CONSTANT_Class), false));
                break;
            /*
             * Multidimensional array of references.
             */
            case Const.MULTIANEWARRAY: {
                index = bytes.readUnsignedShort();
                final int dimensions = bytes.readUnsignedByte();
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(Utility.compactClassName(constantPool.getConstantString(index, Const.CONSTANT_Class), false) + " " + dimensions);
            }
            break;
            /*
             * Increment local variable.
             */
            case Const.IINC:
                if (wide) {
                    vindex = bytes.readUnsignedShort();
                    constant = bytes.readShort();
                    wide = false;
                } else {
                    vindex = bytes.readUnsignedByte();
                    constant = bytes.readByte();
                }
                methodCodeInstruction = new MethodCodeInstructionArg();
                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(vindex + " " + constant);
                break;
            default:
                if (Const.getNoOfOperands(opcode) > 0) {
                    for (int i = 0; i < Const.getOperandTypeCount(opcode); i++) {
                        switch (Const.getOperandType(opcode, i)) {
                            case Const.T_BYTE:
                                methodCodeInstruction = new MethodCodeInstructionArg();
                                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(String.valueOf(bytes.readByte()));
                                break;
                            case Const.T_SHORT:
                                methodCodeInstruction = new MethodCodeInstructionArg();
                                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(String.valueOf(bytes.readShort()));
                                break;
                            case Const.T_INT:
                                methodCodeInstruction = new MethodCodeInstructionArg();
                                ((MethodCodeInstructionArg) methodCodeInstruction).setArg(String.valueOf(bytes.readInt()));
                                break;
                            default: // Never reached
                                throw new IllegalStateException("Unreachable default case reached!");
                        }
                    }
                }
        }
        if (methodCodeInstruction == null) {
            methodCodeInstruction = new MethodCodeInstruction();
        }
        if (methodCodeInstruction.getInstructionName() == null) {
            methodCodeInstruction.setInstructionName(instructionName);
        }
        return methodCodeInstruction;
    }
}
