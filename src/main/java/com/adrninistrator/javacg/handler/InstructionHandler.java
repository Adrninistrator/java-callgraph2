package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.constant.ConstElementDouble;
import com.adrninistrator.javacg.dto.element.constant.ConstElementFloat;
import com.adrninistrator.javacg.dto.element.constant.ConstElementInt;
import com.adrninistrator.javacg.dto.element.constant.ConstElementLong;
import com.adrninistrator.javacg.dto.element.constant.ConstElementNull;
import com.adrninistrator.javacg.dto.element.constant.ConstElementString;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.JSRElement;
import com.adrninistrator.javacg.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldMethodCallElement;
import com.adrninistrator.javacg.dto.element.variable.VariableElement;
import com.adrninistrator.javacg.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg.dto.frame.FieldInformation;
import com.adrninistrator.javacg.dto.frame.JavaCGLocalVariables;
import com.adrninistrator.javacg.dto.frame.JavaCGOperandStack;
import com.adrninistrator.javacg.dto.instruction.BaseInstructionParseResult;
import com.adrninistrator.javacg.dto.instruction.MethodCallParseResult;
import com.adrninistrator.javacg.dto.instruction.RetParseResult;
import com.adrninistrator.javacg.dto.instruction.ReturnParseResult;
import com.adrninistrator.javacg.enums.ConstantTypeEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.LocalVariable;
import org.apache.bcel.classfile.LocalVariableTable;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.ANEWARRAY;
import org.apache.bcel.generic.BIPUSH;
import org.apache.bcel.generic.CHECKCAST;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.GETFIELD;
import org.apache.bcel.generic.GETSTATIC;
import org.apache.bcel.generic.IINC;
import org.apache.bcel.generic.INVOKEDYNAMIC;
import org.apache.bcel.generic.INVOKEINTERFACE;
import org.apache.bcel.generic.INVOKESPECIAL;
import org.apache.bcel.generic.INVOKESTATIC;
import org.apache.bcel.generic.INVOKEVIRTUAL;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InvokeInstruction;
import org.apache.bcel.generic.LDC;
import org.apache.bcel.generic.LDC2_W;
import org.apache.bcel.generic.LoadInstruction;
import org.apache.bcel.generic.MULTIANEWARRAY;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.NEW;
import org.apache.bcel.generic.NEWARRAY;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.PUTFIELD;
import org.apache.bcel.generic.PUTSTATIC;
import org.apache.bcel.generic.RET;
import org.apache.bcel.generic.SIPUSH;
import org.apache.bcel.generic.StoreInstruction;
import org.apache.bcel.generic.Type;
import org.apache.bcel.generic.TypedInstruction;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/10/16
 * @description: 对指令进行处理
 */
public class InstructionHandler {
    private final MethodGen mg;

    private final ConstantPoolGen cpg;

    private final LocalVariableTable localVariableTable;

    private JavaCGOperandStack stack;

    private JavaCGLocalVariables locals;

    private FieldInformation nonStaticFieldInfo;

    private FieldInformation staticFieldInfo;

    // 解析构造函数以获取非静态字段可能的类型的开关
    private boolean recordFieldPossibleTypeFlag;

    // 使用已获取的构造函数非静态字段可能的类型的开关
    private boolean useFieldPossibleTypeFlag;

    // 非静态字段字段所有可能的类型
    private FieldPossibleTypes nonStaticFieldPossibleTypes;

    public InstructionHandler(MethodGen mg,
                              LocalVariableTable localVariableTable,
                              JavaCGOperandStack stack,
                              JavaCGLocalVariables locals,
                              FieldInformation nonStaticFieldInfo,
                              FieldInformation staticFieldInfo) {
        this.mg = mg;
        this.cpg = mg.getConstantPool();
        this.localVariableTable = localVariableTable;
        this.stack = stack;
        this.locals = locals;
        this.nonStaticFieldInfo = nonStaticFieldInfo;
        this.staticFieldInfo = staticFieldInfo;
    }

    private String getTypeString(TypedInstruction typedInstruction) {
        return typedInstruction.getType(cpg).toString();
    }

    public BaseInstructionParseResult parse(InstructionHandle ih) {
        Instruction i = ih.getInstruction();
        int opCode = i.getOpcode();
        BaseInstructionParseResult instructionParseResult = null;

        switch (opCode) {
            case Const.NOP:
                // 什么也不做
                break;
            case Const.ACONST_NULL:
                stack.push(new ConstElementNull());
                break;
            case Const.ICONST_M1:
            case Const.ICONST_0:
            case Const.ICONST_1:
            case Const.ICONST_2:
            case Const.ICONST_3:
            case Const.ICONST_4:
            case Const.ICONST_5:
                stack.push(new ConstElementInt(opCode - Const.ICONST_M1 - 1));
                break;
            case Const.LCONST_0:
            case Const.LCONST_1:
                stack.push(new ConstElementLong((long) (opCode - Const.LCONST_0)));
                break;
            case Const.FCONST_0:
                stack.push(new ConstElementFloat(0.0F));
                break;
            case Const.FCONST_1:
                stack.push(new ConstElementFloat(1.0F));
                break;
            case Const.FCONST_2:
                stack.push(new ConstElementFloat(2.0F));
                break;
            case Const.DCONST_0:
                stack.push(new ConstElementDouble(0.0D));
                break;
            case Const.DCONST_1:
                stack.push(new ConstElementDouble(1.0D));
                break;
            case Const.BIPUSH:
                stack.push(new ConstElementInt(((BIPUSH) i).getValue()));
                break;
            case Const.SIPUSH:
                stack.push(new ConstElementInt(((SIPUSH) i).getValue()));
                break;
            case Const.LDC:
            case Const.LDC_W:
                handleLDC((LDC) i);
                break;
            case Const.LDC2_W:
                handleLDC2W((LDC2_W) i);
                break;
            case Const.ILOAD:
            case Const.LLOAD:
            case Const.FLOAD:
            case Const.DLOAD:
            case Const.ALOAD:
            case Const.ILOAD_0:
            case Const.ILOAD_1:
            case Const.ILOAD_2:
            case Const.ILOAD_3:
            case Const.LLOAD_0:
            case Const.LLOAD_1:
            case Const.LLOAD_2:
            case Const.LLOAD_3:
            case Const.FLOAD_0:
            case Const.FLOAD_1:
            case Const.FLOAD_2:
            case Const.FLOAD_3:
            case Const.DLOAD_0:
            case Const.DLOAD_1:
            case Const.DLOAD_2:
            case Const.DLOAD_3:
            case Const.ALOAD_0:
            case Const.ALOAD_1:
            case Const.ALOAD_2:
            case Const.ALOAD_3:
                handleLoad((LoadInstruction) i);
                break;
            case Const.IALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.AALOAD:
                handleArrayLoad(null, true);
                break;
            case Const.BALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_BYTE);
                break;
            case Const.CALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_CHAR);
                break;
            case Const.SALOAD:
                handleArrayLoad(ConstantTypeEnum.CONSTTE_SHORT);
                break;
            case Const.ISTORE:
            case Const.ISTORE_0:
            case Const.ISTORE_1:
            case Const.ISTORE_2:
            case Const.ISTORE_3:
                handleStore((StoreInstruction) i, ConstantTypeEnum.CONSTTE_INT, ih);
                break;
            case Const.LSTORE:
            case Const.LSTORE_0:
            case Const.LSTORE_1:
            case Const.LSTORE_2:
            case Const.LSTORE_3:
                handleStore((StoreInstruction) i, ConstantTypeEnum.CONSTTE_LONG, ih);
                break;
            case Const.FSTORE:
            case Const.FSTORE_0:
            case Const.FSTORE_1:
            case Const.FSTORE_2:
            case Const.FSTORE_3:
                handleStore((StoreInstruction) i, ConstantTypeEnum.CONSTTE_FLOAT, ih);
                break;
            case Const.DSTORE:
            case Const.DSTORE_0:
            case Const.DSTORE_1:
            case Const.DSTORE_2:
            case Const.DSTORE_3:
                handleStore((StoreInstruction) i, ConstantTypeEnum.CONSTTE_DOUBLE, ih);
                break;
            case Const.ASTORE:
            case Const.ASTORE_0:
            case Const.ASTORE_1:
            case Const.ASTORE_2:
            case Const.ASTORE_3:
                handleStore((StoreInstruction) i, null, ih);
                break;
            case Const.IASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.AASTORE:
                handleArrayStore(null, true);
                break;
            case Const.BASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_BYTE);
                break;
            case Const.CASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_CHAR);
                break;
            case Const.SASTORE:
                handleArrayStore(ConstantTypeEnum.CONSTTE_SHORT);
                break;
            case Const.POP:
                stack.pop();
                break;
            case Const.POP2:
                handlePOP2();
                break;
            case Const.DUP:
                handleDUP();
                break;
            case Const.DUP_X1:
                handleDUPX1();
                break;
            case Const.DUP_X2:
                handleDUPX2();
                break;
            case Const.DUP2:
                handleDUP2();
                break;
            case Const.DUP2_X1:
                handleDUP2X1();
                break;
            case Const.DUP2_X2:
                handleDUP2X2();
                break;
            case Const.SWAP:
                handleSWAP();
                break;
            case Const.IADD:
            case Const.ISUB:
            case Const.IMUL:
            case Const.IDIV:
            case Const.IREM:
            case Const.IAND:
            case Const.IOR:
            case Const.IXOR:
                handleArithmeticOperation2(ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LADD:
            case Const.LSUB:
            case Const.LMUL:
            case Const.LDIV:
            case Const.LREM:
            case Const.LAND:
            case Const.LOR:
            case Const.LXOR:
                handleArithmeticOperation2(ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FADD:
            case Const.FSUB:
            case Const.FMUL:
            case Const.FDIV:
            case Const.FREM:
                handleArithmeticOperation2(ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DADD:
            case Const.DSUB:
            case Const.DMUL:
            case Const.DDIV:
            case Const.DREM:
                handleArithmeticOperation2(ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.INEG:
                handleArithmeticOperation1(ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LNEG:
                handleArithmeticOperation1(ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FNEG:
                handleArithmeticOperation1(ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DNEG:
                handleArithmeticOperation1(ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.ISHL:
            case Const.ISHR:
            case Const.IUSHR:
                handleBitOperator(ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LSHL:
            case Const.LSHR:
            case Const.LUSHR:
                handleBitOperator(ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.IINC:
                handleIinc((IINC) i);
                break;
            case Const.I2L:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_INT, ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.I2F:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_INT, ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.I2D:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_INT, ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.L2I:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_LONG, ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.L2F:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_LONG, ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.L2D:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_LONG, ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.F2I:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_FLOAT, ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.F2L:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_FLOAT, ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.F2D:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_FLOAT, ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.D2I:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_DOUBLE, ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.D2L:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_DOUBLE, ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.D2F:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_DOUBLE, ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.I2B:
            case Const.I2C:
            case Const.I2S:
                handleTypeConversion(ConstantTypeEnum.CONSTTE_INT, ConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LCMP:
                handleCompare(ConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FCMPL:
            case Const.FCMPG:
                handleCompare(ConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DCMPL:
            case Const.DCMPG:
                handleCompare(ConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.IFEQ:
            case Const.IFNE:
            case Const.IFLT:
            case Const.IFGE:
            case Const.IFGT:
            case Const.IFLE:
                handleIf1();
                break;
            case Const.IF_ICMPEQ:
            case Const.IF_ICMPNE:
            case Const.IF_ICMPLT:
            case Const.IF_ICMPGE:
            case Const.IF_ICMPGT:
            case Const.IF_ICMPLE:
                handleIf2();
                break;
            case Const.IF_ACMPEQ:
            case Const.IF_ACMPNE:
                stack.pop();
                stack.pop();
                break;
            case Const.GOTO:
                // 操作数栈无变化
                break;
            case Const.JSR:
                handleJSR(ih);
                break;
            case Const.RET:
                instructionParseResult = handleRET((RET) i);
                break;
            case Const.TABLESWITCH:
            case Const.LOOKUPSWITCH:
                stack.pop();
                break;
            case Const.IRETURN:
            case Const.LRETURN:
            case Const.FRETURN:
            case Const.DRETURN:
            case Const.ARETURN:
                instructionParseResult = handleReturn();
                break;
            case Const.RETURN:
                // 操作数栈无变化
                break;
            case Const.GETSTATIC:
                handleGETSTATIC((GETSTATIC) i);
                break;
            case Const.PUTSTATIC:
                handlePUTSTATIC((PUTSTATIC) i);
                break;
            case Const.GETFIELD:
                handleGETFIELD((GETFIELD) i);
                break;
            case Const.PUTFIELD:
                handlePUTFIELD((PUTFIELD) i);
                break;
            case Const.INVOKEVIRTUAL:
                instructionParseResult = handleMethodInvoke((INVOKEVIRTUAL) i, true);
                break;
            case Const.INVOKESPECIAL:
                instructionParseResult = handleMethodInvoke((INVOKESPECIAL) i, true);
                break;
            case Const.INVOKESTATIC:
                instructionParseResult = handleMethodInvoke((INVOKESTATIC) i, false);
                break;
            case Const.INVOKEINTERFACE:
                instructionParseResult = handleMethodInvoke((INVOKEINTERFACE) i, true);
                break;
            case Const.INVOKEDYNAMIC:
                instructionParseResult = handleMethodInvoke((INVOKEDYNAMIC) i, false);
                break;
            case Const.NEW:
                stack.push(new VariableElement(getTypeString((NEW) i)));
                break;
            case Const.NEWARRAY:
                handleNEWARRAY((NEWARRAY) i);
                break;
            case Const.ANEWARRAY:
                handleANEWARRAY((ANEWARRAY) i);
                break;
            case Const.ARRAYLENGTH:
                stack.pop();
                stack.push(new VariableElement(ConstantTypeEnum.CONSTTE_INT.getType()));
                break;
            case Const.ATHROW:
                handleATHROW();
                break;
            case Const.CHECKCAST:
                stack.pop();
                stack.push(new VariableElement(getTypeString((CHECKCAST) i)));
                break;
            case Const.INSTANCEOF:
                stack.pop();
                stack.push(new VariableElement(ConstantTypeEnum.CONSTTE_INT.getType()));
                break;
            case Const.MONITORENTER:
            case Const.MONITOREXIT:
                stack.pop();
                break;
            case Const.WIDE:
                // 操作数栈无变化
                break;
            case Const.MULTIANEWARRAY:
                handleMULTIANEWARRAY((MULTIANEWARRAY) i);
                break;
            case Const.IFNULL:
            case Const.IFNONNULL:
                stack.pop();
                break;
            case Const.GOTO_W:
                // 操作数栈无变化
                break;
            case Const.JSR_W:
                handleJSR(ih);
                break;
            default:
                System.err.println("eee 未处理的指令 " + opCode + " " + i.getClass().getName());
                break;
        }

        return instructionParseResult;
    }

    private void handleLDC(LDC ldc) {
        String typeString = getTypeString(ldc);
        Object value = ldc.getValue(cpg);

        if (ConstantTypeEnum.CONSTTE_INT.getType().equals(typeString)) {
            stack.push(new ConstElementInt(value));
        } else if (ConstantTypeEnum.CONSTTE_FLOAT.getType().equals(typeString)) {
            stack.push(new ConstElementFloat(value));
        } else if (JavaCGCommonNameConstants.CLASS_NAME_STRING.equals(typeString)) {
            stack.push(new ConstElementString(value));
        } else if (JavaCGCommonNameConstants.CLASS_NAME_CLASS.equals(typeString)) {
            ObjectType objectType = (ObjectType) value;
            stack.push(new VariableElement(objectType.getClassName()));
        } else {
            System.err.println("eee LDC 暂不支持的情况 " + typeString + " " + value);
        }
    }

    private void handleLDC2W(LDC2_W ldc2W) {
        String typeString = getTypeString(ldc2W);
        Object value = ldc2W.getValue(cpg);

        if (ConstantTypeEnum.CONSTTE_LONG.getType().equals(typeString)) {
            stack.push(new ConstElementLong(value));
        } else if (ConstantTypeEnum.CONSTTE_DOUBLE.getType().equals(typeString)) {
            stack.push(new ConstElementDouble(value));
        } else {
            System.err.println("eee LDC2_W 暂不支持的情况 " + typeString + " " + value);
        }
    }

    // 处理获取值
    private void handleLoad(LoadInstruction loadInstruction) {
        int index = loadInstruction.getIndex();
        if (locals.size() > index) {
            stack.push(locals.get(index));
            return;
        }

        throw new JavaCGRuntimeException("本地变量不存在 " + index);
    }

    // 处理数组获取值
    private void handleArrayLoad(ConstantTypeEnum typeEnum) {
        handleArrayLoad(typeEnum.getType(), false);
    }

    private void handleArrayLoad(String elementType, boolean isAaload) {
        // 出栈，数组索引
        BaseElement indexVariable = stack.pop();
        // 出栈，数组对象
        BaseElement arrayVariable = stack.pop();

        String usedElementType;
        if (!isAaload) {
            usedElementType = elementType;
        } else {
            String arrayType = arrayVariable.getType();
            if (!JavaCGByteCodeUtil.isNullType(arrayType) && !JavaCGByteCodeUtil.isArrayType(arrayType)) {
                throw new JavaCGRuntimeException("当前类型不是数组 " + arrayType);
            }
            usedElementType = JavaCGByteCodeUtil.removeArrayFlag(arrayType);
        }

        // 入栈，数组元素
        stack.push(new VariableElement(usedElementType));

        indexVariable.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
        if (!isAaload) {
            arrayVariable.checkTypeString(JavaCGByteCodeUtil.addArrayFlag(usedElementType));
        }
    }

    // 处理赋值
    private void handleStore(StoreInstruction storeInstruction, ConstantTypeEnum typeEnum, InstructionHandle ih) {
        int index = storeInstruction.getIndex();
        BaseElement baseElement = stack.pop();
        String poppedElementType = baseElement.getType();

        if (typeEnum == null) {
            // 处理ASTORE指令
            if (!JavaCGByteCodeUtil.isNullType(poppedElementType)) {
                // 操作数栈中元素类型有值
                // 添加本地变量
                locals.add(poppedElementType, baseElement, index);
                return;
            }
        }

        // 处理非ASTORE指令，或操作数栈中元素类型未获取到值
        // 从LocalVariableTable中获取本地变量对应类型
        LocalVariable localVariable = localVariableTable.getLocalVariable(index, ih.getNext().getPosition());
        String actualType;
        if (localVariable != null) {
            actualType = Utility.typeSignatureToString(localVariable.getSignature(), false);
        } else {
            actualType = (typeEnum != null ? typeEnum.getType() : poppedElementType);
        }

        // 添加本地变量
        locals.add(actualType, baseElement, index);
    }

    // 处理数组赋值
    private void handleArrayStore(ConstantTypeEnum typeEnum) {
        handleArrayStore(typeEnum.getType(), false);
    }

    private void handleArrayStore(String arrayType, boolean isAastore) {
        BaseElement valueVariable = stack.pop();
        BaseElement indexVariable = stack.pop();
        BaseElement arrayVariable = stack.pop();

        indexVariable.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());

        if (!isAastore) {
            valueVariable.checkTypeString(arrayType);
            arrayVariable.checkTypeString(JavaCGByteCodeUtil.addArrayFlag(arrayType));
        }
    }

    private void handlePOP2() {
        BaseElement element = stack.pop();
        if (element.getElementSize() == 1) {
            stack.pop();
        }
    }

    private void handleDUP() {
        BaseElement element = stack.pop();
        stack.push(element);
        stack.push(element);
    }

    private void handleDUPX1() {
        BaseElement element1 = stack.pop();
        BaseElement element2 = stack.pop();
        stack.push(element1);
        stack.push(element2);
        stack.push(element1);
    }

    private void handleDUPX2() {
        BaseElement element1 = stack.pop();
        BaseElement element2 = stack.pop();
        if (element2.getElementSize() == 2) {
            stack.push(element1);
            stack.push(element2);
            stack.push(element1);
            return;
        }

        BaseElement element3 = stack.pop();
        stack.push(element1);
        stack.push(element3);
        stack.push(element2);
        stack.push(element1);
    }

    private void handleDUP2() {
        BaseElement element1 = stack.pop();
        if (element1.getElementSize() == 2) {
            stack.push(element1);
            stack.push(element1);
            return;
        }

        BaseElement element2 = stack.pop();
        stack.push(element2);
        stack.push(element1);
        stack.push(element2);
        stack.push(element1);
    }

    private void handleDUP2X1() {
        BaseElement element1 = stack.pop();
        BaseElement element2 = stack.pop();
        if (element1.getElementSize() == 2) {
            stack.push(element1);
            stack.push(element2);
            stack.push(element1);
            return;
        }

        BaseElement element3 = stack.pop();
        stack.push(element2);
        stack.push(element1);
        stack.push(element3);
        stack.push(element2);
        stack.push(element1);
    }

    private void handleDUP2X2() {
        BaseElement element1 = stack.pop();
        BaseElement element2 = stack.pop();
        if (element1.getElementSize() == 2) {
            if (element2.getElementSize() == 2) {
                stack.push(element1);
                stack.push(element2);
                stack.push(element1);
                return;
            }

            BaseElement element3 = stack.pop();
            stack.push(element1);
            stack.push(element3);
            stack.push(element2);
            stack.push(element1);
            return;
        }

        BaseElement element3 = stack.pop();
        if (element3.getElementSize() == 2) {
            stack.push(element2);
            stack.push(element1);
            stack.push(element3);
            stack.push(element2);
            stack.push(element1);
            return;
        }

        BaseElement element4 = stack.pop();
        stack.push(element2);
        stack.push(element1);
        stack.push(element4);
        stack.push(element3);
        stack.push(element2);
        stack.push(element1);
    }

    private void handleSWAP() {
        BaseElement element1 = stack.pop();
        BaseElement element2 = stack.pop();
        stack.push(element1);
        stack.push(element2);
    }

    // 处理双目运算
    private void handleArithmeticOperation2(ConstantTypeEnum constantTypeEnum) {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        stack.push(new VariableElement(constantTypeEnum.getType()));

        value2.checkTypeString(constantTypeEnum.getType());
        value1.checkTypeString(constantTypeEnum.getType());
    }

    // 处理单目运算
    private void handleArithmeticOperation1(ConstantTypeEnum constantTypeEnum) {
        BaseElement value = stack.pop();
        stack.push(new VariableElement(constantTypeEnum.getType()));

        value.checkTypeString(constantTypeEnum.getType());
    }

    // 处理位运算
    private void handleBitOperator(ConstantTypeEnum constantTypeEnum) {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        stack.push(new VariableElement(constantTypeEnum.getType()));

        value2.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
        value1.checkTypeString(constantTypeEnum.getType());
    }

    // 处理IINC指令
    private void handleIinc(IINC iinc) {
        // 将指定的本地变量值设为null
        locals.setValue2Null(iinc.getIndex());

        // 操作数栈无变化
    }

    // 处理类型转换
    private void handleTypeConversion(ConstantTypeEnum oldTypeEnum, ConstantTypeEnum newTypeEnum) {
        BaseElement value = stack.pop();
        stack.push(new VariableElement(newTypeEnum.getType()));

        value.checkTypeString(oldTypeEnum.getType());
    }

    // 处理比较判断
    private void handleCompare(ConstantTypeEnum constantTypeEnum) {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        stack.push(new VariableElement(ConstantTypeEnum.CONSTTE_INT.getType()));

        value2.checkTypeString(constantTypeEnum.getType());
        value1.checkTypeString(constantTypeEnum.getType());
    }

    // 处理if判断，1个元素
    private void handleIf1() {
        BaseElement value = stack.pop();
        value.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
    }

    // 处理if判断，2个元素
    private void handleIf2() {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        value2.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
        value1.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
    }

    private void handleJSR(InstructionHandle ih) {
        stack.push(new JSRElement(ih.getNext()));
    }

    private RetParseResult handleRET(RET ret) {
        // 根据ret指令索引获取对应的本地变量
        LocalVariableElement localVariableElement = locals.get(ret.getIndex());

        if (!JavaCGConstants.JSR_TYPE.equals(localVariableElement.getType()) ||
                !(localVariableElement.getValue() instanceof InstructionHandle)) {
            throw new JavaCGRuntimeException("ret指令对应的本地变量非法 " + localVariableElement);
        }

        InstructionHandle jsrNextIh = (InstructionHandle) localVariableElement.getValue();
        return new RetParseResult(jsrNextIh);
    }

    private ReturnParseResult handleReturn() {
        BaseElement baseElement = stack.pop();
        return new ReturnParseResult(baseElement);
    }

    private void handleGETSTATIC(GETSTATIC getstatic) {
        String fieldName = getstatic.getFieldName(cpg);
        ObjectType classType = getstatic.getLoadClassType(cpg);

        FieldElement fieldElement = staticFieldInfo.getStatic(classType.getClassName(), fieldName);
        if (fieldElement == null) {
            fieldElement = new StaticFieldElement(getstatic.getFieldType(cpg).toString(), null, fieldName, classType.getClassName());
        }

        stack.push(fieldElement);
    }

    private void handlePUTSTATIC(PUTSTATIC putstatic) {
        String fieldName = putstatic.getFieldName(cpg);
        ObjectType classType = putstatic.getLoadClassType(cpg);
        BaseElement value = stack.pop();

        StaticFieldElement staticFieldElement = new StaticFieldElement(value.getType(), value.getValue(), fieldName, classType.getClassName());
        staticFieldInfo.putStatic(classType.getClassName(), fieldName, staticFieldElement);
    }

    private void handleGETFIELD(GETFIELD getfield) {
        String fieldName = getfield.getFieldName(cpg);
        BaseElement object = stack.pop();

        FieldElement fieldElement = null;
        if (object instanceof LocalVariableElement) {
            LocalVariableElement localVariableElement = (LocalVariableElement) object;
            if (localVariableElement.getIndex() == 0) {
                // 仅处理对this的变量获取
                fieldElement = nonStaticFieldInfo.get(fieldName);
                if (fieldElement != null) {
                    stack.push(fieldElement);
                    return;
                }
            }
        }

        if (useFieldPossibleTypeFlag) {
            // 使用已获取的构造函数非静态字段可能的类型
//			org.apache.poi.openxml4j.opc.ZipPackage 类的构建函数解析后 zipArchive 非静态字段可能的类型数量大于1 null org.apache.poi.openxml4j.util.ZipInputStreamZipEntrySource
            List<String> possibleTypeList = nonStaticFieldPossibleTypes.getPossibleTypeList(fieldName);
            if (possibleTypeList != null && !possibleTypeList.isEmpty()) {
                if (possibleTypeList.size() == 1) {
                    // 字段可能的类型数量为1，则使用
                    fieldElement = new FieldElement(possibleTypeList.get(0), null, fieldName);
                } else {
                    // 字段可能的类型数量大于1，无法使用
                    System.err.println(mg.getClassName() + " 类的构选函数解析后 " + fieldName + " 非静态字段可能的类型数量大于1 " + StringUtils.join(possibleTypeList, " "));
                }
            }
        }

        if (fieldElement == null) {
            fieldElement = new FieldElement(getfield.getFieldType(cpg).toString(), null, fieldName);
        }
        stack.push(fieldElement);
    }

    private void handlePUTFIELD(PUTFIELD putfield) {
        String fieldName = putfield.getFieldName(cpg);
        BaseElement value = stack.pop();
        BaseElement object = stack.pop();

        if (object instanceof LocalVariableElement) {
            LocalVariableElement localVariableElement = (LocalVariableElement) object;
            if (localVariableElement.getIndex() == 0) {
                // 仅处理对this的变量赋值
                FieldElement fieldElement = new FieldElement(value.getType(), value.getValue(), fieldName);
                nonStaticFieldInfo.put(fieldName, fieldElement);
                if (recordFieldPossibleTypeFlag) {
                    String rawFieldType = putfield.getFieldType(cpg).toString();
                    if (!StringUtils.equals(rawFieldType, value.getType())) {
                        // 记录非静态字段可能的类型，仅当字段实际类型与原始类型不相同时才记录
                        nonStaticFieldPossibleTypes.addPossibleType(fieldName, value.getType());
                    }
                }
            }
        }
    }

    // 处理方法调用指令
    private MethodCallParseResult handleMethodInvoke(InvokeInstruction invokeInstruction, boolean userObjectReference) {
        // 处理参数
        int argumentNumber = invokeInstruction.getArgumentTypes(cpg).length;
        List<BaseElement> argumentList = new ArrayList<>(argumentNumber);
        for (int i = 0; i < argumentNumber; i++) {
            // 参数逆序处理
            argumentList.add(0, stack.pop());
        }

        BaseElement objectElement = null;
        if (userObjectReference) {
            // 方法调用需要使用被调用对象
            objectElement = stack.pop();
        }

        // 处理返回值
        Type returnType = invokeInstruction.getReturnType(cpg);
        if (returnType != Type.VOID) {
            VariableElement variableElement;
            if (objectElement instanceof StaticFieldElement) {
                StaticFieldElement staticFieldElement = (StaticFieldElement) objectElement;
                variableElement = new StaticFieldMethodCallElement(returnType.toString(), staticFieldElement.getClassName(), staticFieldElement.getFieldName(),
                        invokeInstruction.getMethodName(cpg));
            } else {
                variableElement = new VariableElement(returnType.toString());
            }
            stack.push(variableElement);
        }

        return new MethodCallParseResult(objectElement, argumentList);
    }

    private void handleNEWARRAY(NEWARRAY newarray) {
        BaseElement countVariable = stack.pop();
        stack.push(new VariableElement(newarray.getType().toString()));

        countVariable.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
    }

    private void handleANEWARRAY(ANEWARRAY anewarray) {
        BaseElement countVariable = stack.pop();
        stack.push(new VariableElement(JavaCGByteCodeUtil.addArrayFlag(getTypeString(anewarray))));

        countVariable.checkTypeString(ConstantTypeEnum.CONSTTE_INT.getType());
    }

    private void handleATHROW() {
        BaseElement throwableElement = stack.pop();
        stack.push(throwableElement);
    }

    private void handleMULTIANEWARRAY(MULTIANEWARRAY multianewarray) {
        for (int i = 0; i < multianewarray.getDimensions(); i++) {
            stack.pop();
        }
        stack.push(new VariableElement(getTypeString(multianewarray)));
    }

    //
    public void setStack(JavaCGOperandStack stack) {
        this.stack = stack;
    }

    public void setLocals(JavaCGLocalVariables locals) {
        this.locals = locals;
    }

    public void setNonStaticFieldInfo(FieldInformation nonStaticFieldInfo) {
        this.nonStaticFieldInfo = nonStaticFieldInfo;
    }

    public void setStaticFieldInfo(FieldInformation staticFieldInfo) {
        this.staticFieldInfo = staticFieldInfo;
    }

    public void setRecordFieldPossibleTypeFlag(boolean recordFieldPossibleTypeFlag) {
        this.recordFieldPossibleTypeFlag = recordFieldPossibleTypeFlag;
    }

    public void setUseFieldPossibleTypeFlag(boolean useFieldPossibleTypeFlag) {
        this.useFieldPossibleTypeFlag = useFieldPossibleTypeFlag;
    }

    public void setNonStaticFieldPossibleTypes(FieldPossibleTypes nonStaticFieldPossibleTypes) {
        this.nonStaticFieldPossibleTypes = nonStaticFieldPossibleTypes;
    }
}
