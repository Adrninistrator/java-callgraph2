package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGArithmeticOperationTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
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
import com.adrninistrator.javacg.dto.frame.FieldInformationMap;
import com.adrninistrator.javacg.dto.frame.JavaCGLocalVariables;
import com.adrninistrator.javacg.dto.frame.JavaCGOperandStack;
import com.adrninistrator.javacg.dto.instruction.parseresult.AThrowNullParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.AThrowParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.BaseInstructionParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.MethodCallParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.PutFieldParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.PutStaticParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.RetParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.ReturnParseResult;
import com.adrninistrator.javacg.dto.variabledatasource.VariableDataSourceArithmeticOperation;
import com.adrninistrator.javacg.dto.variabledatasource.VariableDataSourceField;
import com.adrninistrator.javacg.dto.variabledatasource.VariableDataSourceMethodArg;
import com.adrninistrator.javacg.dto.variabledatasource.VariableDataSourceMethodCallReturn;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGElementUtil;
import com.adrninistrator.javacg.util.JavaCGInstructionUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.LocalVariable;
import org.apache.bcel.classfile.LocalVariableTable;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.ANEWARRAY;
import org.apache.bcel.generic.ArrayType;
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
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/10/16
 * @description: 对指令进行处理
 */
public class InstructionHandler {
    private static final Logger logger = LoggerFactory.getLogger(InstructionHandler.class);

    private final Map<String, Map<String, Integer>> frEqConversionMethodMap;

    private final MethodGen mg;

    private final ConstantPoolGen cpg;

    private final LocalVariableTable localVariableTable;

    private JavaCGOperandStack stack;

    private JavaCGLocalVariables locals;

    private FieldInformationMap nonStaticFieldInfoMap;

    private FieldInformationMap staticFieldInfoMap;

    // 需要解析方法调用的可能的类型与值的开关
    protected boolean parseMethodCallTypeValueFlag;

    // 解析构造函数以获取非静态字段可能的类型的开关
    private boolean recordFieldPossibleTypeFlag;

    // 使用已获取的构造函数非静态字段可能的类型的开关
    private boolean useFieldPossibleTypeFlag;

    // 非静态字段字段所有可能的类型
    private FieldPossibleTypes nonStaticFieldPossibleTypes;

    // 当前方法是否可能为set方法
    private boolean maybeSetMethod;

    // 当前方法是否为静态代码块
    private boolean inClinitMethod;

    public InstructionHandler(JavaCGConfInfo javaCGConfInfo,
                              MethodGen mg,
                              LocalVariableTable localVariableTable,
                              JavaCGOperandStack stack,
                              JavaCGLocalVariables locals,
                              FieldInformationMap nonStaticFieldInfoMap,
                              FieldInformationMap staticFieldInfoMap) {
        this.frEqConversionMethodMap = javaCGConfInfo.getFrEqConversionMethodMap();
        this.mg = mg;
        this.cpg = mg.getConstantPool();
        this.localVariableTable = localVariableTable;
        this.stack = stack;
        this.locals = locals;
        this.nonStaticFieldInfoMap = nonStaticFieldInfoMap;
        this.staticFieldInfoMap = staticFieldInfoMap;
    }

    public BaseInstructionParseResult parse(InstructionHandle ih) {
        Instruction i = ih.getInstruction();
        short opCode = i.getOpcode();
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
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LALOAD:
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FALOAD:
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DALOAD:
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.AALOAD:
                handleArrayLoad(null, true);
                break;
            case Const.BALOAD:
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_BYTE);
                break;
            case Const.CALOAD:
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_CHAR);
                break;
            case Const.SALOAD:
                handleArrayLoad(JavaCGConstantTypeEnum.CONSTTE_SHORT);
                break;
            case Const.ISTORE:
            case Const.ISTORE_0:
            case Const.ISTORE_1:
            case Const.ISTORE_2:
            case Const.ISTORE_3:
                handleStore((StoreInstruction) i, JavaCGConstantTypeEnum.CONSTTE_INT, ih);
                break;
            case Const.LSTORE:
            case Const.LSTORE_0:
            case Const.LSTORE_1:
            case Const.LSTORE_2:
            case Const.LSTORE_3:
                handleStore((StoreInstruction) i, JavaCGConstantTypeEnum.CONSTTE_LONG, ih);
                break;
            case Const.FSTORE:
            case Const.FSTORE_0:
            case Const.FSTORE_1:
            case Const.FSTORE_2:
            case Const.FSTORE_3:
                handleStore((StoreInstruction) i, JavaCGConstantTypeEnum.CONSTTE_FLOAT, ih);
                break;
            case Const.DSTORE:
            case Const.DSTORE_0:
            case Const.DSTORE_1:
            case Const.DSTORE_2:
            case Const.DSTORE_3:
                handleStore((StoreInstruction) i, JavaCGConstantTypeEnum.CONSTTE_DOUBLE, ih);
                break;
            case Const.ASTORE:
            case Const.ASTORE_0:
            case Const.ASTORE_1:
            case Const.ASTORE_2:
            case Const.ASTORE_3:
                handleStore((StoreInstruction) i, null, ih);
                break;
            case Const.IASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.AASTORE:
                handleArrayStore(null, true);
                break;
            case Const.BASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_BYTE);
                break;
            case Const.CASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_CHAR);
                break;
            case Const.SASTORE:
                handleArrayStore(JavaCGConstantTypeEnum.CONSTTE_SHORT);
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
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGArithmeticOperationTypeEnum.AOTE_ADD);
                break;
            case Const.ISUB:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGArithmeticOperationTypeEnum.AOTE_SUB);
                break;
            case Const.IMUL:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGArithmeticOperationTypeEnum.AOTE_MUL);
                break;
            case Const.IDIV:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGArithmeticOperationTypeEnum.AOTE_DIV);
                break;
            case Const.IREM:
            case Const.IAND:
            case Const.IOR:
            case Const.IXOR:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_INT, null);
                break;
            case Const.LADD:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGArithmeticOperationTypeEnum.AOTE_ADD);
                break;
            case Const.LSUB:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGArithmeticOperationTypeEnum.AOTE_SUB);
                break;
            case Const.LMUL:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGArithmeticOperationTypeEnum.AOTE_MUL);
                break;
            case Const.LDIV:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGArithmeticOperationTypeEnum.AOTE_DIV);
                break;
            case Const.LREM:
            case Const.LAND:
            case Const.LOR:
            case Const.LXOR:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_LONG, null);
                break;
            case Const.FADD:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGArithmeticOperationTypeEnum.AOTE_ADD);
                break;
            case Const.FSUB:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGArithmeticOperationTypeEnum.AOTE_SUB);
                break;
            case Const.FMUL:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGArithmeticOperationTypeEnum.AOTE_MUL);
                break;
            case Const.FDIV:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGArithmeticOperationTypeEnum.AOTE_DIV);
                break;
            case Const.FREM:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_FLOAT, null);
                break;
            case Const.DADD:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGArithmeticOperationTypeEnum.AOTE_ADD);
                break;
            case Const.DSUB:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGArithmeticOperationTypeEnum.AOTE_SUB);
                break;
            case Const.DMUL:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGArithmeticOperationTypeEnum.AOTE_MUL);
                break;
            case Const.DDIV:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGArithmeticOperationTypeEnum.AOTE_DIV);
                break;
            case Const.DREM:
                handleArithmeticOperation2(ih, JavaCGConstantTypeEnum.CONSTTE_DOUBLE, null);
                break;
            case Const.INEG:
                handleArithmeticOperation1(JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LNEG:
                handleArithmeticOperation1(JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FNEG:
                handleArithmeticOperation1(JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DNEG:
                handleArithmeticOperation1(JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.ISHL:
            case Const.ISHR:
            case Const.IUSHR:
                handleBitOperator(JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LSHL:
            case Const.LSHR:
            case Const.LUSHR:
                handleBitOperator(JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.IINC:
                handleIinc((IINC) i);
                break;
            case Const.I2L:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.I2F:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.I2D:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.L2I:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.L2F:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.L2D:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_LONG, JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.F2I:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.F2L:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.F2D:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_FLOAT, JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
                break;
            case Const.D2I:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.D2L:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.D2F:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_DOUBLE, JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.I2B:
            case Const.I2C:
            case Const.I2S:
                handleTypeConversion(JavaCGConstantTypeEnum.CONSTTE_INT, JavaCGConstantTypeEnum.CONSTTE_INT);
                break;
            case Const.LCMP:
                handleCompare(JavaCGConstantTypeEnum.CONSTTE_LONG);
                break;
            case Const.FCMPL:
            case Const.FCMPG:
                handleCompare(JavaCGConstantTypeEnum.CONSTTE_FLOAT);
                break;
            case Const.DCMPL:
            case Const.DCMPG:
                handleCompare(JavaCGConstantTypeEnum.CONSTTE_DOUBLE);
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
                instructionParseResult = handlePUTSTATIC((PUTSTATIC) i);
                break;
            case Const.GETFIELD:
                handleGETFIELD((GETFIELD) i);
                break;
            case Const.PUTFIELD:
                instructionParseResult = handlePUTFIELD((PUTFIELD) i);
                break;
            case Const.INVOKEVIRTUAL:
                instructionParseResult = handleMethodInvoke((INVOKEVIRTUAL) i, ih, true);
                break;
            case Const.INVOKESPECIAL:
                instructionParseResult = handleMethodInvoke((INVOKESPECIAL) i, ih, true);
                break;
            case Const.INVOKESTATIC:
                instructionParseResult = handleMethodInvoke((INVOKESTATIC) i, ih, false);
                break;
            case Const.INVOKEINTERFACE:
                instructionParseResult = handleMethodInvoke((INVOKEINTERFACE) i, ih, true);
                break;
            case Const.INVOKEDYNAMIC:
                instructionParseResult = handleMethodInvoke((INVOKEDYNAMIC) i, ih, false);
                break;
            case Const.NEW:
                stack.push(new VariableElement(JavaCGInstructionUtil.getTypeString((NEW) i, cpg)));
                break;
            case Const.NEWARRAY:
                handleNEWARRAY((NEWARRAY) i);
                break;
            case Const.ANEWARRAY:
                handleANEWARRAY((ANEWARRAY) i);
                break;
            case Const.ARRAYLENGTH:
                stack.pop();
                stack.push(new VariableElement(JavaCGConstantTypeEnum.CONSTTE_INT.getType()));
                break;
            case Const.ATHROW:
                instructionParseResult = handleATHROW();
                break;
            case Const.CHECKCAST:
                handleCHECKCAST((CHECKCAST) i);
                break;
            case Const.INSTANCEOF:
                stack.pop();
                stack.push(new VariableElement(JavaCGConstantTypeEnum.CONSTTE_INT.getType()));
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
                logger.error("未处理的指令 {} {}", opCode, i.getClass().getName());
                break;
        }

        return instructionParseResult;
    }

    private void handleLDC(LDC ldc) {
        String typeString = JavaCGInstructionUtil.getTypeString(ldc, cpg);
        Object value = ldc.getValue(cpg);

        if (JavaCGConstantTypeEnum.CONSTTE_INT.getType().equals(typeString)) {
            stack.push(new ConstElementInt(value));
        } else if (JavaCGConstantTypeEnum.CONSTTE_FLOAT.getType().equals(typeString)) {
            stack.push(new ConstElementFloat(value));
        } else if (JavaCGCommonNameConstants.CLASS_NAME_STRING.equals(typeString)) {
            stack.push(new ConstElementString(value));
        } else if (JavaCGCommonNameConstants.CLASS_NAME_CLASS.equals(typeString)) {
            ObjectType objectType = (ObjectType) value;
            stack.push(new VariableElement(objectType.getClassName()));
        } else {
            logger.error("LDC 暂不支持的情况 {} {}", typeString, value);
        }
    }

    private void handleLDC2W(LDC2_W ldc2W) {
        String typeString = JavaCGInstructionUtil.getTypeString(ldc2W, cpg);
        Object value = ldc2W.getValue(cpg);

        if (JavaCGConstantTypeEnum.CONSTTE_LONG.getType().equals(typeString)) {
            stack.push(new ConstElementLong(value));
        } else if (JavaCGConstantTypeEnum.CONSTTE_DOUBLE.getType().equals(typeString)) {
            stack.push(new ConstElementDouble(value));
        } else {
            logger.error("LDC2_W 暂不支持的情况 {} {}", typeString, value);
        }
    }

    // 处理获取值
    private void handleLoad(LoadInstruction loadInstruction) {
        int loadIndex = loadInstruction.getIndex();
        if (locals.size() > loadIndex) {
            LocalVariableElement local = locals.get(loadIndex);
            // 获取LOAD指令对应的方法参数下标
            int argIndex = JavaCGByteCodeUtil.getArgIndexInMethod(mg, loadIndex);
            if (argIndex >= 0) {
                // 当前LOAD指令是获取方法参数
                VariableDataSourceMethodArg variableDataSourceMethodArg = new VariableDataSourceMethodArg(JavaCGConstants.METHOD_CALL_ARGUMENTS_START_SEQ + argIndex,
                        local.getType());
                // 记录变量的数据来源
                local.recordVariableDataSource(variableDataSourceMethodArg, frEqConversionMethodMap);
            }
            stack.push(local);
            return;
        }

        throw new JavaCGRuntimeException("本地变量不存在 " + loadIndex);
    }

    // 处理数组获取值
    private void handleArrayLoad(JavaCGConstantTypeEnum typeEnum) {
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
        stack.push(new VariableElement(usedElementType, true));

        indexVariable.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
        if (!isAaload) {
            arrayVariable.checkTypeString(JavaCGByteCodeUtil.addArrayFlag(usedElementType));
        }
    }

    // 处理赋值
    private void handleStore(StoreInstruction storeInstruction, JavaCGConstantTypeEnum typeEnum, InstructionHandle ih) {
        int index = storeInstruction.getIndex();
        BaseElement baseElement = stack.pop();
        String poppedElementType = baseElement.getType();

        // 从LocalVariableTable中获取本地变量对应类型
        LocalVariable localVariable = localVariableTable.getLocalVariable(index, ih.getNext().getPosition());
        String variableName = (localVariable != null ? localVariable.getName() : null);
        if (typeEnum == null) {
            // 处理ASTORE指令
            if (!JavaCGByteCodeUtil.isNullType(poppedElementType)) {
                // 操作数栈中元素类型有值
                // 添加本地变量
                locals.add(poppedElementType, baseElement, index, variableName);
                return;
            }
        }

        // 处理非ASTORE指令，或操作数栈中元素类型未获取到值
        String actualType;
        if (localVariable != null) {
            actualType = Utility.typeSignatureToString(localVariable.getSignature(), false);
        } else {
            actualType = (typeEnum != null ? typeEnum.getType() : poppedElementType);
        }

        // 添加本地变量
        locals.add(actualType, baseElement, index, variableName);
    }

    // 处理数组赋值
    private void handleArrayStore(JavaCGConstantTypeEnum typeEnum) {
        handleArrayStore(typeEnum.getType(), false);
    }

    private void handleArrayStore(String arrayType, boolean isAastore) {
        BaseElement valueVariable = stack.pop();
        BaseElement indexVariable = stack.pop();
        BaseElement arrayVariable = stack.pop();

        indexVariable.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());

        if (!isAastore) {
            valueVariable.checkTypeString(arrayType);
            arrayVariable.checkTypeString(JavaCGByteCodeUtil.addArrayFlag(arrayType));
        }

        if (!arrayVariable.isArrayElement()) {
            logger.error("不是数组类型 {}", arrayVariable.getClass().getName());
            return;
        }

        Object indexObj = indexVariable.getValue();
        if (indexObj instanceof Integer) {
            // 数组赋值的元素下标是常量
            Integer index = (Integer) indexObj;
            // 记录数组指定下标的元素
            arrayVariable.setElement(index, valueVariable);
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

    // 处理二元运算
    private void handleArithmeticOperation2(InstructionHandle ih, JavaCGConstantTypeEnum constantTypeEnum, JavaCGArithmeticOperationTypeEnum arithmeticOperationTypeEnum) {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        value2.checkTypeString(constantTypeEnum.getType());
        value1.checkTypeString(constantTypeEnum.getType());

        VariableElement variableElement = new VariableElement(constantTypeEnum.getType());
        if (arithmeticOperationTypeEnum != null) {
            // 处理算术运算数据来源
            VariableDataSourceArithmeticOperation dataSourceArithmeticOperation = new VariableDataSourceArithmeticOperation(ih.getPosition(), arithmeticOperationTypeEnum,
                    value1, value2);
            variableElement.setVariableDataSource(dataSourceArithmeticOperation);
        }
        stack.push(variableElement);
    }

    // 处理一元运算
    private void handleArithmeticOperation1(JavaCGConstantTypeEnum constantTypeEnum) {
        BaseElement value = stack.pop();
        stack.push(new VariableElement(constantTypeEnum.getType()));

        value.checkTypeString(constantTypeEnum.getType());
    }

    // 处理位运算
    private void handleBitOperator(JavaCGConstantTypeEnum constantTypeEnum) {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        stack.push(new VariableElement(constantTypeEnum.getType()));

        value2.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
        value1.checkTypeString(constantTypeEnum.getType());
    }

    // 处理IINC指令
    private void handleIinc(IINC iinc) {
        // 将指定的本地变量值设为null
        locals.setValue2Null(iinc.getIndex());

        // 操作数栈无变化
    }

    // 处理类型转换
    private void handleTypeConversion(JavaCGConstantTypeEnum oldTypeEnum, JavaCGConstantTypeEnum newTypeEnum) {
        BaseElement value = stack.pop();
        stack.push(new VariableElement(newTypeEnum.getType()));

        value.checkTypeString(oldTypeEnum.getType());
    }

    // 处理比较判断
    private void handleCompare(JavaCGConstantTypeEnum constantTypeEnum) {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        stack.push(new VariableElement(JavaCGConstantTypeEnum.CONSTTE_INT.getType()));

        value2.checkTypeString(constantTypeEnum.getType());
        value1.checkTypeString(constantTypeEnum.getType());
    }

    // 处理if判断，1个元素
    private void handleIf1() {
        BaseElement value = stack.pop();
        value.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
    }

    // 处理if判断，2个元素
    private void handleIf2() {
        BaseElement value2 = stack.pop();
        BaseElement value1 = stack.pop();
        value2.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
        value1.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
    }

    private void handleJSR(InstructionHandle ih) {
        stack.push(new JSRElement(ih.getNext()));
    }

    private RetParseResult handleRET(RET ret) {
        // 根据RET指令索引获取对应的本地变量
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
        if (parseMethodCallTypeValueFlag) {
            return new ReturnParseResult(baseElement);
        }
        return null;
    }

    private void handleGETSTATIC(GETSTATIC getstatic) {
        String fieldName = getstatic.getFieldName(cpg);
        ObjectType classType = getstatic.getLoadClassType(cpg);

        FieldElement fieldElement = staticFieldInfoMap.getStatic(classType.getClassName(), fieldName);
        if (fieldElement == null) {
            // 假如是静态字段定义的类型与实际初始化类型不一致，则以上获取的fieldElement会是null，因为代码中没有对静态初始化方法进行处理（意义不大）
            String fieldType = getstatic.getFieldType(cpg).toString();
            fieldElement = new StaticFieldElement(fieldType, false, null, fieldName, classType.getClassName());
        }
        stack.push(fieldElement);
    }

    private PutStaticParseResult handlePUTSTATIC(PUTSTATIC putstatic) {
        String fieldName = putstatic.getFieldName(cpg);
        ObjectType classType = putstatic.getLoadClassType(cpg);
        BaseElement value = stack.pop();

        String putStaticClassName = classType.getClassName();
        StaticFieldElement staticFieldElement = new StaticFieldElement(value.getType(), value.isArrayElement(), value.getValue(), fieldName, putStaticClassName);
        // 数组类型的处理
        if (value.isArrayElement()) {
            staticFieldElement.setArrayValueMap(value.getArrayValueMap());
        }

        staticFieldInfoMap.putStatic(putStaticClassName, fieldName, staticFieldElement);

        if (inClinitMethod) {
            // 仅当在静态代码块时返回以下解析结果
            return new PutStaticParseResult(putStaticClassName, fieldName, putstatic.getFieldType(cpg).toString(), value);
        }
        return null;
    }

    private void handleGETFIELD(GETFIELD getfield) {
        String fieldName = getfield.getFieldName(cpg);
        BaseElement object = stack.pop();
        if (!(object instanceof VariableElement)) {
            throw new JavaCGRuntimeException("GETFIELD对象类型与预期不一致: " + object.getClass().getName());
        }

        // 获取字段所在的类的类名
        String objectClassName = JavaCGElementUtil.getVariableClassNameOrThis((VariableElement) object);
        // 准备变量的数据来源，类名与字段名称是确定的
        VariableDataSourceField variableDataSourceField = new VariableDataSourceField();
        variableDataSourceField.setClassName(objectClassName);
        variableDataSourceField.setFieldName(fieldName);

        if (object instanceof LocalVariableElement) {
            LocalVariableElement objectLocalVariableElement = (LocalVariableElement) object;
            FieldElement fieldElement;
            if (objectLocalVariableElement.isThis()) {
                // 处理对this的变量获取
                // 尝试从已处理的非静态变量中获取
                fieldElement = nonStaticFieldInfoMap.get(fieldName);
                if (fieldElement != null) {
                    FieldElement newFieldElement = fieldElement.copyFieldElement();
                    variableDataSourceField.setFieldType(newFieldElement.getType());
                    // 记录变量的数据来源
                    newFieldElement.recordVariableDataSource(variableDataSourceField, frEqConversionMethodMap);
                    stack.push(newFieldElement);
                    return;
                }

                if (useFieldPossibleTypeFlag) {
                    // 从已处理的非静态变量中未获取到，使用当前类已获取的构造函数非静态字段可能的类型
                    List<String> possibleTypeList = nonStaticFieldPossibleTypes.getPossibleTypeList(fieldName);
                    if (!JavaCGUtil.isCollectionEmpty(possibleTypeList)) {
                        if (possibleTypeList.size() == 1) {
                            // 字段可能的类型数量为1，则使用
                            fieldElement = new FieldElement(possibleTypeList.get(0), false, null, fieldName, objectClassName);
                            variableDataSourceField.setFieldType(fieldElement.getType());
                            // 记录变量的数据来源
                            fieldElement.recordVariableDataSource(variableDataSourceField, frEqConversionMethodMap);
                            stack.push(fieldElement);
                            return;
                        }
                        // 字段可能的类型数量大于1，无法使用
                        logger.warn("{} 类的构造函数解析后 {} 非静态字段可能的类型数量大于1 {}", mg.getClassName(), fieldName, StringUtils.join(possibleTypeList, " "));
                    }
                }
            }
        }

        // 若以上未得到字段合适的类型，则使用GETFIELD指令对应的类型
        String fieldType = getfield.getFieldType(cpg).toString();
        FieldElement fieldElement = new FieldElement(fieldType, false, null, fieldName, objectClassName);
        variableDataSourceField.setFieldType(fieldElement.getType());
        // 记录变量的数据来源
        fieldElement.recordVariableDataSource(variableDataSourceField, frEqConversionMethodMap);
        stack.push(fieldElement);
    }

    private PutFieldParseResult handlePUTFIELD(PUTFIELD putfield) {
        String fieldName = putfield.getFieldName(cpg);
        BaseElement value = stack.pop();
        BaseElement object = stack.pop();
        if (!(object instanceof VariableElement)) {
            throw new JavaCGRuntimeException("PUTFIELD对象类型与预期不一致: " + object.getClass().getName());
        }

        if (object instanceof LocalVariableElement) {
            LocalVariableElement objectLocalVariableElement = (LocalVariableElement) object;
            if (objectLocalVariableElement.isThis()) {
                // 仅处理对this的变量赋值
                String objectClassName = JavaCGElementUtil.getVariableClassNameOrThis(objectLocalVariableElement);
                FieldElement fieldElement = new FieldElement(value.getType(), value.isArrayElement(), value.getValue(), fieldName, objectClassName);
                // 数组类型的处理
                if (value.isArrayElement()) {
                    fieldElement.setArrayValueMap(value.getArrayValueMap());
                }

                nonStaticFieldInfoMap.put(fieldName, fieldElement);
                if (recordFieldPossibleTypeFlag) {
                    String rawFieldType = putfield.getFieldType(cpg).toString();
                    if (!StringUtils.equals(rawFieldType, value.getType())) {
                        // 记录非静态字段可能的类型，仅当字段实际类型与原始类型不相同时才记录
                        nonStaticFieldPossibleTypes.addPossibleType(fieldName, value.getType());
                    }
                }
            }
        }

        if (maybeSetMethod) {
            return new PutFieldParseResult(fieldName, putfield.getFieldType(cpg).toString(), value, object);
        }
        return null;
    }

    // 处理方法调用指令
    private MethodCallParseResult handleMethodInvoke(InvokeInstruction invokeInstruction, InstructionHandle ih, boolean userObjectReference) {
        // 处理参数
        Type[] argTypes = invokeInstruction.getArgumentTypes(cpg);
        int argumentNumber = argTypes.length;
        List<BaseElement> argElementList = new ArrayList<>(argumentNumber);
        for (int i = 0; i < argumentNumber; i++) {
            // 参数逆序处理
            argElementList.add(0, stack.pop());
        }

        BaseElement objectElement = null;
        if (userObjectReference) {
            // 方法调用需要使用被调用对象
            objectElement = stack.pop();
        }

        // 处理返回值
        Type returnType = invokeInstruction.getReturnType(cpg);
        String calleeClassName = invokeInstruction.getReferenceType(cpg).toString();
        String calleeMethodName = invokeInstruction.getMethodName(cpg);
        if (Type.VOID != returnType) {
            // 被调用方法返回类型非void
            VariableElement variableElement;
            if (objectElement instanceof StaticFieldElement) {
                // 被调用对象属于静态字段
                StaticFieldElement staticFieldElement = (StaticFieldElement) objectElement;
                variableElement = new StaticFieldMethodCallElement(returnType.toString(), (returnType instanceof ArrayType), staticFieldElement.getClassName(),
                        staticFieldElement.getName(), calleeMethodName);
            } else {
                variableElement = new VariableElement(returnType.toString());
            }
            VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = new VariableDataSourceMethodCallReturn(ih.getPosition(),
                    invokeInstruction.getClass().getSimpleName(), calleeClassName, calleeMethodName, JavaCGClassMethodUtil.getArgTypeStr(argTypes), returnType.toString(),
                    objectElement, argElementList);

            // 记录变量的数据来源
            variableElement.recordVariableDataSource(variableDataSourceMethodCallReturn, frEqConversionMethodMap);
            stack.push(variableElement);
        } else if (JavaCGClassMethodUtil.isInitMethod(calleeMethodName) && !JavaCGClassMethodUtil.isObjectClass(calleeClassName) && !stack.isEmpty()) {
            // 调用构造函数，且不是调用Object的构造函数，且操作数栈非空时处理
            BaseElement topElement = stack.peek();
            if (topElement instanceof VariableElement) {
                // 调用构造函数时，栈中的元素有可能不是变量
                VariableElement topVariableElement = (VariableElement) topElement;
                // 修改数据来源
                VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = new VariableDataSourceMethodCallReturn(ih.getPosition(),
                        invokeInstruction.getClass().getSimpleName(), calleeClassName, calleeMethodName, JavaCGClassMethodUtil.getArgTypeStr(argTypes), calleeClassName,
                        objectElement, argElementList);
                topVariableElement.recordVariableDataSource(variableDataSourceMethodCallReturn, frEqConversionMethodMap);
            }
        }

        return new MethodCallParseResult(objectElement, argElementList);
    }

    private void handleNEWARRAY(NEWARRAY newarray) {
        BaseElement countVariable = stack.pop();
        VariableElement arrayElement = new VariableElement(newarray.getType().toString(), true);
        stack.push(arrayElement);

        countVariable.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
    }

    private void handleANEWARRAY(ANEWARRAY anewarray) {
        BaseElement countVariable = stack.pop();
        VariableElement arrayElement = new VariableElement(JavaCGByteCodeUtil.addArrayFlag(JavaCGInstructionUtil.getTypeString(anewarray, cpg)), true);
        stack.push(arrayElement);

        countVariable.checkTypeString(JavaCGConstantTypeEnum.CONSTTE_INT.getType());
    }

    private AThrowParseResult handleATHROW() {
        BaseElement throwElement = stack.pop();
        if (throwElement instanceof ConstElementNull) {
            // throw null写法对应的情况
            return new AThrowNullParseResult();
        }

        if (!(throwElement instanceof VariableElement)) {
            throw new JavaCGRuntimeException("抛出异常的元素类型不是变量 " + throwElement.getClass().getName());
        }
        VariableElement variableElement = (VariableElement) throwElement;
        stack.push(variableElement);
        return new AThrowParseResult(variableElement);
    }

    private void handleCHECKCAST(CHECKCAST checkcast) {
        BaseElement elementBefore = stack.pop();
        VariableElement variableElementAfter = new VariableElement(JavaCGInstructionUtil.getTypeString(checkcast, cpg));
        // 拷贝变量数据来源
        variableElementAfter.copyVariableDataSource(elementBefore);
        stack.push(variableElementAfter);
    }

    private void handleMULTIANEWARRAY(MULTIANEWARRAY multianewarray) {
        for (int i = 0; i < multianewarray.getDimensions(); i++) {
            stack.pop();
        }
        VariableElement arrayElement = new VariableElement(JavaCGInstructionUtil.getTypeString(multianewarray, cpg), true);
        stack.push(arrayElement);
    }

    //
    public void setStack(JavaCGOperandStack stack) {
        this.stack = stack;
    }

    public void setLocals(JavaCGLocalVariables locals) {
        this.locals = locals;
    }

    public void setNonStaticFieldInfoMap(FieldInformationMap nonStaticFieldInfoMap) {
        this.nonStaticFieldInfoMap = nonStaticFieldInfoMap;
    }

    public void setStaticFieldInfoMap(FieldInformationMap staticFieldInfoMap) {
        this.staticFieldInfoMap = staticFieldInfoMap;
    }

    public void setParseMethodCallTypeValueFlag(boolean parseMethodCallTypeValueFlag) {
        this.parseMethodCallTypeValueFlag = parseMethodCallTypeValueFlag;
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

    public void setMaybeSetMethod(boolean maybeSetMethod) {
        this.maybeSetMethod = maybeSetMethod;
    }

    public void setInClinitMethod(boolean inClinitMethod) {
        this.inClinitMethod = inClinitMethod;
    }
}
