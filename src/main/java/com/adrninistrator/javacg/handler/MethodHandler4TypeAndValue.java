package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGFieldRelationshipTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.branch.BranchStackEntry;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleInfo;
import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg.dto.element.variable.VariableElement;
import com.adrninistrator.javacg.dto.exception.ExceptionTargetInfo;
import com.adrninistrator.javacg.dto.exception.ThrowInfo;
import com.adrninistrator.javacg.dto.exception.ThrowInfoList;
import com.adrninistrator.javacg.dto.fieldrelationship.GetSetFieldRelationship;
import com.adrninistrator.javacg.dto.frame.FieldInformationMap;
import com.adrninistrator.javacg.dto.frame.FrameSnapshotEntry;
import com.adrninistrator.javacg.dto.frame.FrameSnapshotsOfIhs;
import com.adrninistrator.javacg.dto.frame.InstructionStepList;
import com.adrninistrator.javacg.dto.frame.JavaCGLocalVariables;
import com.adrninistrator.javacg.dto.frame.JavaCGOperandStack;
import com.adrninistrator.javacg.dto.instruction.InvokeInstructionPosAndCallee;
import com.adrninistrator.javacg.dto.instruction.parseresult.AThrowNullParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.AThrowParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.BaseInstructionParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.MethodCallParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.PutFieldParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.PutStaticParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.RetParseResult;
import com.adrninistrator.javacg.dto.instruction.parseresult.ReturnParseResult;
import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.dto.variabledatasource.AbstractVariableDataSource;
import com.adrninistrator.javacg.dto.variabledatasource.VariableDataSourceMethodArg;
import com.adrninistrator.javacg.dto.variabledatasource.VariableDataSourceMethodCallReturn;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGElementUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGInstructionUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LocalVariableTable;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.BranchInstruction;
import org.apache.bcel.generic.CodeExceptionGen;
import org.apache.bcel.generic.GotoInstruction;
import org.apache.bcel.generic.IfInstruction;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InvokeInstruction;
import org.apache.bcel.generic.JsrInstruction;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.Select;
import org.apache.bcel.generic.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/2
 * @description: 对方法进行处理，用于获取方法调用指令对应的类型与值
 */
public class MethodHandler4TypeAndValue extends AbstractMethodHandler {

    private static final Logger logger = LoggerFactory.getLogger(MethodHandler4TypeAndValue.class);

    // 指令处理类
    private InstructionHandler instructionHandler;

    // 操作数栈
    private JavaCGOperandStack stack;

    // 本地变量
    private JavaCGLocalVariables locals;

    // 非静态变量
    private FieldInformationMap nonStaticFieldInfoMap;

    // 静态变量
    private FieldInformationMap staticFieldInfoMap;

    // 跳转指令的目标指令位置
    private Set<Integer> jumpTargetIhPositionSet;

    // 用于遍历分支指令的栈
    private ListAsStack<BranchStackEntry> branchStack;

    /*
        分支指令对应的栈桢信息快照
        key     分支指令位置
        value   栈桢信息快照
     */
    private Map<Integer, FrameSnapshotEntry> frameSnapshotMap4Branch;

    // 跳转目标指令与对应的栈桢信息快照列表
    private FrameSnapshotsOfIhs frameSnapshotsOfIhs4JumpTargets;

    /*
        异常处理的跳转信息
        key     Exception table的结束指令位置
        value   Exception table的target指令及异常类型列表
     */
    private Map<Integer, List<ExceptionTargetInfo>> exceptionTargetMap;

    // 异常对应的栈桢信息快照
    private FrameSnapshotsOfIhs frameSnapshotsOfIhs4Exceptions;

    // 指令执行的步骤
    private InstructionStepList instructionStepList;

    /*
        分支指令对应的执行步骤
        key     分支指令位置
        value   指令执行的步骤
     */
    private Map<Integer, InstructionStepList> instructionStepMap4Branch;

    /*
        方法调用可能的信息Map
        key     方法调用指令位置
        value   方法调用可能的信息
     */
    private Map<Integer, MethodCallPossibleInfo> methodCallPossibleInfoMap;

    /*
        方法抛出异常可能的信息Map
        key     方法调用指令位置
        value   方法抛出异常可能的信息
     */
    private Map<Integer, ThrowInfoList> methodThrowPossibleInfoMap;

    // 方法可能的返回信息列表
    private List<BaseElement> returnPossibleInfoList;

    // 解析构造函数以获取非静态字段可能的类型的开关
    private boolean recordFieldPossibleTypeFlag;

    // 使用已获取的构造函数非静态字段可能的类型的开关
    private boolean useFieldPossibleTypeFlag;

    // 需要分析dto的字段之间的关联关系的开关
    private boolean analyseFieldRelationshipFlag;

    // 是否只需要获取可能的返回类型的开关
    private boolean onlyAnalyseReturnTypeFlag;

    // 当前方法是否可能为get方法
    private boolean maybeGetMethod;

    // 当前方法是否可能为set方法
    private boolean maybeSetMethod;

    // 当前方法参数1的类型（当前方法可能为set方法时使用）
    private String methodArg1Type;

    /*
        保存PUTFIELD指令相关的信息
        key     对应的类名
        value   类名对应的字段名称信息
     */
    private Map<String, Set<String>> putFieldClassMap;

    /*
        返回数据的数据来源是GETFIELD指令时的信息
        key     对应的类名
        value   类名对应的字段名称信息
     */
    private Map<String, Set<String>> returnDataSourceGetFieldMap;

    /*
        记录每个方法调用指令对应的通过get/set方法关联的字段关系
        key     方法调用指令位置
        value   get方法对应的数据来源指令位置
     */
    private Map<Integer, Set<Integer>> getSetMethodCallMap;

    public MethodHandler4TypeAndValue(Method method, MethodGen mg, JavaClass javaClass, String callerFullMethod, JavaCGConfInfo javaCGConfInfo) {
        super(method, mg, javaClass, callerFullMethod, javaCGConfInfo);
    }

    /**
     * 方法预处理
     *
     * @return false: 方法不需要继续处理 true: 方法需要继续处理
     */
    @Override
    protected boolean preHandleMethod() {
        // 初始化当前处理的指令
        ih = JavaCGInstructionUtil.getFirstInstructionHandle(mg);
        if (ih == null) {
            // 方法中指令为空，不需要再判断方法是否为abstract或native
            return false;
        }

        // 记录跳转指令的目标指令
        recordJumpTargetInstructionHandle();

        // 记录异常处理的跳转信息
        recordExceptionJumpInfo();
        return true;
    }

    // 记录跳转指令的目标指令
    private void recordJumpTargetInstructionHandle() {
        for (InstructionHandle tempIh : mg.getInstructionList()) {
            Instruction instruction = tempIh.getInstruction();

            // 添加分支指令的目标指令
            if (instruction instanceof BranchInstruction) {
                BranchInstruction branchInstruction = (BranchInstruction) instruction;
                addJumpTargetIhPosition2Set(branchInstruction.getTarget().getPosition());
            }

            // 添加switch类指令的所有目标指令
            if (JavaCGInstructionUtil.isSwitchInstruction(instruction.getOpcode())) {
                Select switchInstruction = (Select) instruction;
                for (InstructionHandle target : switchInstruction.getTargets()) {
                    addJumpTargetIhPosition2Set(target.getPosition());
                }
            }
        }

        if (jumpTargetIhPositionSet != null) {
            frameSnapshotsOfIhs4JumpTargets = new FrameSnapshotsOfIhs();
        }
    }

    private void addJumpTargetIhPosition2Set(int position) {
        if (jumpTargetIhPositionSet == null) {
            jumpTargetIhPositionSet = new HashSet<>();
        }
        jumpTargetIhPositionSet.add(position);
    }

    // 记录异常处理的跳转信息
    private void recordExceptionJumpInfo() {
        CodeExceptionGen[] codeExceptionGens = mg.getExceptionHandlers();
        if (codeExceptionGens.length <= 0) {
            return;
        }

        /*
            key
                Exception table中结束的位置
            value
                实际使用的Exception table中结束的位置
         */
        Map<Integer, Integer> exceptionTableEndMap = new HashMap<>();
        exceptionTargetMap = new HashMap<>();

        for (CodeExceptionGen codeExceptionGen : codeExceptionGens) {
            int exceptionTableEndPosition = codeExceptionGen.getEndPC().getPosition();
            if (exceptionTableEndPosition == codeExceptionGen.getHandlerPC().getPosition()) {
                int exceptionTableBeforeEndPosition = codeExceptionGen.getEndPC().getPrev().getPosition();
                logger.debug("修改使用的Exception table的结束位置 {} {}", exceptionTableEndPosition, exceptionTableBeforeEndPosition);
                exceptionTableEndMap.put(exceptionTableEndPosition, exceptionTableBeforeEndPosition);
            }
        }

        for (CodeExceptionGen codeExceptionGen : codeExceptionGens) {
            // 处理Exception table的结束的位置
            int exceptionTableEndPosition = codeExceptionGen.getEndPC().getPosition();
            Integer actualExceptionTableEndPosition = exceptionTableEndMap.get(exceptionTableEndPosition);
            int usedExceptionTableEndPosition = (actualExceptionTableEndPosition != null ? actualExceptionTableEndPosition : exceptionTableEndPosition);
            List<ExceptionTargetInfo> exceptionTargetList = exceptionTargetMap.computeIfAbsent(usedExceptionTableEndPosition, k -> new ArrayList<>());

            // 处理异常类型
            ObjectType catchExceptionType = codeExceptionGen.getCatchType();
            String exceptionType = (catchExceptionType != null ? catchExceptionType.getClassName() : "");

            // 添加Exception table的target指令及异常类型
            ExceptionTargetInfo exceptionTargetInfo = new ExceptionTargetInfo(codeExceptionGen.getHandlerPC(), exceptionType);
            exceptionTargetList.add(exceptionTargetInfo);
        }

        frameSnapshotsOfIhs4Exceptions = new FrameSnapshotsOfIhs();
    }

    // 判断当前处理的指令是否为Exception table的结束指令
    private boolean checkExceptionEnd(int position) {
        if (exceptionTargetMap == null) {
            return false;
        }

        return exceptionTargetMap.get(position) != null;
    }

    @Override
    protected boolean doHandleMethod() throws IOException {
        if (recordFieldPossibleTypeFlag) {
            logger.debug("预处理构造函数 {}", callerFullMethod);
        }

        // 处理正常指令
        handleAllInstruction();

        // 处理异常处理catch中的指令
        handleExceptionCatchInstructions();
        return true;
    }

    @Override
    protected boolean lastStep() throws IOException {
        if (!onlyAnalyseReturnTypeFlag) {
            // 处理get方法
            handleGetMethod();

            // 处理set方法
            handleSetMethod();
        }
        return true;
    }

    // 初始化
    @Override
    protected void init() {
        LocalVariableTable localVariableTable = mg.getLocalVariableTable(cpg);
        stack = new JavaCGOperandStack(mg.getMaxStack());
        locals = new JavaCGLocalVariables(localVariableTable, mg);
        nonStaticFieldInfoMap = new FieldInformationMap();
        staticFieldInfoMap = new FieldInformationMap();
        frameSnapshotMap4Branch = new HashMap<>();

        if (JavaCGUtil.checkInDebugMode()) {
            instructionStepList = new InstructionStepList();
            instructionStepMap4Branch = new HashMap<>();
        }

        if (parseMethodCallTypeValueFlag) {
            // 需要分析dto的字段之间的关联关系
            if (!mg.isAbstract() && !mg.isStatic() && !javaClass.isEnum()) {
                // 跳过抽象方法、静态方法、枚举类
                // 判断当前方法是否可能为get方法
                if (JavaCGClassMethodUtil.matchesGetMethod(callerMethodName) && Type.VOID != mg.getReturnType() && callerArgTypes.length == 0 &&
                        mg.getInstructionList().size() <= 20) {
                    maybeGetMethod = true;
                    returnDataSourceGetFieldMap = new HashMap<>();
                }
                // 判断当前方法是否可能为set方法
                if (JavaCGClassMethodUtil.matchesSetMethod(callerMethodName) && callerArgTypes.length == 1 && mg.getInstructionList().size() <= 20) {
                    maybeSetMethod = true;
                    putFieldClassMap = new HashMap<>();
                    methodArg1Type = callerArgTypes[0].toString();
                }
            }
        }

        if (analyseFieldRelationshipFlag) {
            getSetMethodCallMap = new HashMap<>();
        }

        instructionHandler = new InstructionHandler(javaCGConfInfo, mg, localVariableTable, stack, locals, nonStaticFieldInfoMap, staticFieldInfoMap);
        instructionHandler.setParseMethodCallTypeValueFlag(parseMethodCallTypeValueFlag);
        instructionHandler.setRecordFieldPossibleTypeFlag(recordFieldPossibleTypeFlag);
        instructionHandler.setUseFieldPossibleTypeFlag(useFieldPossibleTypeFlag);
        instructionHandler.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);
        instructionHandler.setMaybeSetMethod(maybeSetMethod);
        instructionHandler.setInClinitMethod(inClinitMethod);
    }

    // 处理所有的指令
    private void handleAllInstruction() throws IOException {
        while (true) {
            int position = ih.getPosition();
            logger.debug("处理指令 {} ({})", JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih), getSourceLine());

            // 判断跳转目标指令是否跳过处理
            if (skipJumpTargetInstructionHandle()) {
                // 当前跳转目标指令不需要再处理，处理用于遍历分支指令的栈中栈顶元素
                if (handleBranchStackTopEntry()) {
                    // 分支处理完毕
                    break;
                }
                // 分支未处理完毕
                continue;
            }

            if (instructionStepList != null) {
                // 记录指令执行步骤
                instructionStepList.add(ih, getSourceLine());
            }

            // 判断当前处理的指令是否为Exception table的结束指令
            if (checkExceptionEnd(position)) {
                // 当前指令有对应的异常处理，添加信息快照
                logger.debug("异常处理，添加信息快照 {}", JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih));
                frameSnapshotsOfIhs4Exceptions.addSnapshot(position, stack, locals, nonStaticFieldInfoMap, staticFieldInfoMap);
            }

            // 解析指令
            BaseInstructionParseResult instructionParseResult = instructionHandler.parse(ih);

            // 按照不同的分类对指令进行处理
            if (handleInstructionsByType(instructionParseResult)) {
                break;
            }
        }
    }

    /**
     * 按照不同的分类对指令进行处理
     *
     * @param instructionParseResult
     * @return false: 不结束循环 true: 结束循环
     */
    private boolean handleInstructionsByType(BaseInstructionParseResult instructionParseResult) throws IOException {
        short opCode = ih.getInstruction().getOpcode();
        if (JavaCGInstructionUtil.isIfInstruction(opCode)) {
            // 处理IF类指令
            return handleIfInstruction();
        }

        if (JavaCGInstructionUtil.isSwitchInstruction(opCode)) {
            // 处理switch类指令
            return handleSwitchInstruction();
        }

        if (JavaCGInstructionUtil.isGotoInstruction(opCode)) {
            // 处理goto类指令
            handleGotoInstruction();
            return false;
        }

        if (JavaCGInstructionUtil.isExitInstruction(opCode)) {
            if (opCode == Const.ATHROW) {
                // 处理athrow指令
                handleAThrowInstruction((AThrowParseResult) instructionParseResult);
            }

            if (JavaCGInstructionUtil.isReturnWithValueInstruction(opCode)) {
                ReturnParseResult returnParseResult = (ReturnParseResult) instructionParseResult;
                if (parseMethodCallTypeValueFlag) {
                    if (!onlyAnalyseReturnTypeFlag) {
                        // 处理带返回值的return类指令，记录对应的方法参数序号或方法调用指令位置
                        handleReturnInstructionForArgMethodCall(returnParseResult);
                    }

                    if (maybeGetMethod) {
                        // 处理带返回值的return类指令，处理可能的Get方法
                        handleReturnInstruction4GetMethod(returnParseResult);
                    }

                    // 处理带返回值的return类指令，记录对应的返回类型
                    handleReturnInstructionForType(returnParseResult);
                }
            }

            // 处理return、throw类指令
            return handleExitInstruction();
        }

        if (JavaCGInstructionUtil.isJsrInstruction(opCode)) {
            // 处理jsr类指令
            handleJsrInstruction();
            return false;
        }

        if (JavaCGInstructionUtil.isRetInstruction(opCode)) {
            // 处理ret指令
            handleRetInstruction((RetParseResult) instructionParseResult);
            return false;
        }

        if (opCode == Const.PUTFIELD && maybeSetMethod) {
            // 为set方法处理PUTFIELD指令
            handlePutField4SetMethod((PutFieldParseResult) instructionParseResult);
        } else if (opCode == Const.PUTSTATIC && inClinitMethod) {
            // 为静态代码块处理PUTSTATIC指令
            handlePutStatic4Clinit((PutStaticParseResult) instructionParseResult);
        } else if (parseMethodCallTypeValueFlag && JavaCGInstructionUtil.isMethodInvokeInstruction(opCode)) {
            // 处理方法调用指令
            handleInvokeInstruction((MethodCallParseResult) instructionParseResult);
        }

        // 处理下一条指令
        setIh2Next(ih.getNext(), "next");
        return false;
    }

    // 处理异常处理catch中的指令
    private void handleExceptionCatchInstructions() throws IOException {
        if (exceptionTargetMap == null) {
            return;
        }

        // Exception table的结束指令位置列表
        List<Integer> exceptionEndPositionList = new ArrayList<>(exceptionTargetMap.keySet());
        Collections.sort(exceptionEndPositionList);

        for (int exceptionEndPosition : exceptionEndPositionList) {
            logger.debug("处理异常处理指令 [{}]", exceptionEndPosition);

            // Exception table的target指令及异常类型列表
            List<ExceptionTargetInfo> exceptionTargetList = exceptionTargetMap.get(exceptionEndPosition);
            if (exceptionTargetList == null) {
                continue;
            }

            // 异常对应的栈桢信息快照
            List<FrameSnapshotEntry> exceptionEndFrameSnapshotList = frameSnapshotsOfIhs4Exceptions.get(exceptionEndPosition);
            if (exceptionEndFrameSnapshotList == null) {
                continue;
            }

            for (ExceptionTargetInfo exceptionTargetInfo : exceptionTargetList) {
                // 由于exceptionEndFrameSnapshotList的数量会变化，以下for循环需要使用下标遍历
                for (int i = 0; i < exceptionEndFrameSnapshotList.size(); i++) {
                    FrameSnapshotEntry exceptionEndFrameSnapshot = exceptionEndFrameSnapshotList.get(i);
                    ih = exceptionTargetInfo.getTarget();
                    // 重新设置当前使用的栈桢对应的信息，清空操作数栈并在栈顶放入异常类型
                    resetFrameInfo(exceptionEndFrameSnapshot, exceptionTargetInfo);

                    if (instructionStepList != null) {
                        // 清空指令执行的步骤
                        instructionStepList.clear();
                    }

                    // 处理当前catch中的指令
                    handleAllInstruction();
                }
            }
        }
    }

    /**
     * 重新设置当前使用的栈桢对应的信息
     *
     * @param frameSnapshotEntry
     * @param exceptionTargetInfo 异常处理信息，非空时需要清空操作数栈并在栈顶放入异常类型
     */
    private void resetFrameInfo(FrameSnapshotEntry frameSnapshotEntry, ExceptionTargetInfo exceptionTargetInfo) {
        if (exceptionTargetInfo != null) {
            // 清空操作数栈并在栈顶放入异常类型
            stack.clear();
            VariableElement variableElement = new VariableElement(exceptionTargetInfo.getExceptionType());
            variableElement.setCatchExceptionStartPosition(exceptionTargetInfo.getTarget().getPosition());
            stack.push(variableElement);
        } else {
            stack = frameSnapshotEntry.copyStackSnapshot();
        }
        instructionHandler.setStack(stack);

        locals = frameSnapshotEntry.copyLocalsSnapshot();
        instructionHandler.setLocals(locals);

        nonStaticFieldInfoMap = frameSnapshotEntry.copyNonStaticFieldInfo();
        instructionHandler.setNonStaticFieldInfoMap(nonStaticFieldInfoMap);

        staticFieldInfoMap = frameSnapshotEntry.copyStaticFieldInfo();
        instructionHandler.setStaticFieldInfoMap(staticFieldInfoMap);
    }

    private void setIh2Next(InstructionHandle nextIh, String type) {
        ih = nextIh;
        logger.debug("{} 下一条处理的指令 {}", type, ih.getPosition());
    }

    /**
     * 判断跳转目标指令是否跳过处理
     *
     * @return false: 不跳过当前指令的处理 true: 跳过当前指令的处理
     */
    private boolean skipJumpTargetInstructionHandle() {
        int position = ih.getPosition();
        if (jumpTargetIhPositionSet == null || !jumpTargetIhPositionSet.contains(position)) {
            // 当前指令不是跳转目标指令
            return false;
        }

        // 添加信息快照，若有添加，则不跳过当前指令的处理；若未添加，则跳过当前指令的处理
        if (!frameSnapshotsOfIhs4JumpTargets.addSnapshot(position, stack, locals, nonStaticFieldInfoMap, staticFieldInfoMap)) {
            logger.debug("跳转目标指令[跳过]处理 {}", position);
            return true;
        }

        logger.debug("跳转目标指令[需要]处理 {}", position);
        return false;
    }

    /**
     * 处理if类指令
     *
     * @return false: 指令处理未完毕 true: 指令处理完毕
     */
    private boolean handleIfInstruction() {
        IfInstruction ifInstruction = (IfInstruction) ih.getInstruction();

        BranchStackEntry branchStackEntry = new BranchStackEntry(ih, 2);
        branchStackEntry.addTargetIh(ih.getNext());
        branchStackEntry.addTargetIh(ifInstruction.getTarget());

        // 处理分支指令
        return handleBranchInstruction(branchStackEntry);
    }

    /**
     * 处理switch类指令
     *
     * @return false: 指令处理未完毕 true: 指令处理完毕
     */
    private boolean handleSwitchInstruction() {
        Select switchInstruction = (Select) ih.getInstruction();
        InstructionHandle[] targets = switchInstruction.getTargets();

        BranchStackEntry branchStackEntry = new BranchStackEntry(ih, targets.length + 1);
        if (targets.length > 0) {
            for (InstructionHandle target : targets) {
                branchStackEntry.addTargetIh(target);
            }
        }

        branchStackEntry.addTargetIh(switchInstruction.getTarget());

        // 处理分支指令
        return handleBranchInstruction(branchStackEntry);
    }

    /**
     * 处理分支指令
     *
     * @param branchStackEntry
     * @return false: 指令处理未完毕 true: 指令处理完毕
     */
    private boolean handleBranchInstruction(BranchStackEntry branchStackEntry) {
        if (branchStack == null) {
            branchStack = new ListAsStack<>();
        }

        // 检查是否出现循环/递归调用
        int ihPosition = branchStackEntry.getBranchIh().getPosition();
        int head = branchStack.getHead();
        for (int i = 0; i <= head; i++) {
            BranchStackEntry tmpBranchStackEntry = branchStack.getElementAt(i);
            if (ihPosition == tmpBranchStackEntry.getBranchIh().getPosition()) {
                // 当前指令已在栈中，处理用于遍历分支指令的栈中栈顶元素
                return handleBranchStackTopEntry();
            }
        }

        // 用于遍历分支指令的栈入栈
        branchStack.push(branchStackEntry);
        Integer branchStackIndex = branchStack.getHead();

        // 继续处理当前分支指令的第一个目标指令
        setIh2Next(branchStackEntry.getTargetIhList().get(0), "branch");

        // 复制栈桢信息快照，记录与分支指令的关系
        FrameSnapshotEntry frameSnapshotEntry = new FrameSnapshotEntry(stack.copy(), locals.copy(), nonStaticFieldInfoMap.copy(), staticFieldInfoMap.copy());
        frameSnapshotMap4Branch.put(branchStackIndex, frameSnapshotEntry);

        // 复制指令执行步骤，记录与分支指令的关系
        if (instructionStepList != null) {
            instructionStepMap4Branch.put(branchStackIndex, instructionStepList.copy());
        }

        // 指令未处理完毕
        return false;
    }

    // 处理goto类指令
    private void handleGotoInstruction() {
        GotoInstruction gotoInstruction = (GotoInstruction) ih.getInstruction();
        // 处理goto的目标指令
        setIh2Next(gotoInstruction.getTarget(), "goto");
    }

    /**
     * 处理return、throw类指令
     *
     * @return false: 指令处理未完毕 true: 指令处理完毕
     */
    private boolean handleExitInstruction() {
        if (branchStack == null) {
            // 当前方法不需要进行分支处理，返回指令处理完毕
            return true;
        }

        // 处理用于遍历分支指令的栈中栈顶元素
        return handleBranchStackTopEntry();
    }

    // 处理jsr类指令
    private void handleJsrInstruction() {
        JsrInstruction jsrInstruction = (JsrInstruction) ih.getInstruction();
        // 处理jsr的目标指令
        setIh2Next(jsrInstruction.getTarget(), "jsr");
    }

    // 处理ret指令
    private void handleRetInstruction(RetParseResult retParseResult) {
        // 处理jsr的目下一条指令
        setIh2Next(retParseResult.getJsrNextIh(), "jsr_next");
    }

    /**
     * 处理用于遍历分支指令的栈中栈顶元素
     *
     * @return false: 指令处理未完毕 true: 指令处理完毕
     */
    private boolean handleBranchStackTopEntry() {
        if (branchStack == null || branchStack.isEmpty()) {
            // 假如用于遍历分支指令的栈为空，说明可以结束处理
            return true;
        }

        // 获取用于遍历分支指令的栈中栈顶元素
        BranchStackEntry branchStackEntry = branchStack.peek();
        int branchPosition = branchStackEntry.getBranchIh().getPosition();
        logger.debug("处理用于遍历分支指令的栈中栈顶元素 {}", branchPosition);

        Integer branchStackIndex = branchStack.getHead();
        int targetIhIndex = branchStackEntry.getTargetIhIndex();
        targetIhIndex++;

        List<InstructionHandle> targetIhList = branchStackEntry.getTargetIhList();
        if (targetIhIndex >= targetIhList.size()) {
            // 当前分支目标指令已处理完毕

            // 清除分支指令对应的栈桢信息快照
            frameSnapshotMap4Branch.remove(branchStackIndex);

            if (instructionStepList != null) {
                // 清除分支指令对应的指令执行步骤快照
                instructionStepMap4Branch.remove(branchStackIndex);
            }

            // 用于遍历分支指令的栈删除栈顶元素
            branchStack.removeTop();
            if (branchStack.isEmpty()) {
                // 用于遍历分支指令的栈已空，指令处理完毕
                return true;
            }

            // 继续处理用于遍历分支指令的栈中栈顶元素
            return handleBranchStackTopEntry();
        }

        // 当前分支目标指令还未处理完毕
        // 继续处理当前分支指令的下一个目标指令
        setIh2Next(branchStackEntry.getTargetIhList().get(targetIhIndex), "branch_next");

        // 获取分支指令对应的栈桢信息快照
        FrameSnapshotEntry frameSnapshot4Branch = frameSnapshotMap4Branch.get(branchStackIndex);
        // 重新设置当前使用的栈桢对应的信息
        resetFrameInfo(frameSnapshot4Branch, null);

        if (instructionStepList != null) {
            // 恢复本地变量到分支指令对应的情况
            instructionStepList = instructionStepMap4Branch.get(branchStackIndex).copy();
        }

        branchStackEntry.setTargetIhIndex(targetIhIndex);

        // 指令处理未完毕
        return false;
    }

    // 为set方法处理PUTFIELD指令
    private void handlePutField4SetMethod(PutFieldParseResult putFieldParseResult) {
        // 判断PUTFIELD指令是否来自方法参数
        if (!checkLegalSetMethod(putFieldParseResult)) {
            return;
        }

        BaseElement object = putFieldParseResult.getObject();
        if (!(object instanceof VariableElement)) {
            throw new JavaCGRuntimeException("PUTFIELD对象类型与预期不一致: " + object.getClass().getName());
        }
        // 若PUTFIELD写入的值对应的数据来源是方法参数，则满足set方法
        VariableElement objectVariableElement = (VariableElement) object;
        String objectClassName = JavaCGElementUtil.getVariableClassNameOrThis(objectVariableElement);
        // 记录PUTFIELD对应的类及字段名
        Set<String> putFieldFieldNameSet = putFieldClassMap.computeIfAbsent(objectClassName, k -> new HashSet<>());
        putFieldFieldNameSet.add(putFieldParseResult.getFieldName());
    }

    // 判断PUTFIELD指令是否来自方法参数
    private boolean checkLegalSetMethod(PutFieldParseResult putFieldParseResult) {
        BaseElement value = putFieldParseResult.getValue();
        if (!(value instanceof VariableElement)) {
            return false;
        }

        if (JavaCGElementUtil.checkElementDataSourceGetArg(value)) {
            // PUTFIELD的数据来源是方法的参数，满足set方法
            return true;
        }

        VariableElement valueVariableElement = (VariableElement) value;
        AbstractVariableDataSource valueVariableDataSource = valueVariableElement.getVariableDataSource();
        if (!(valueVariableDataSource instanceof VariableDataSourceMethodCallReturn) || !methodArg1Type.equals(putFieldParseResult.getFieldType())) {
            // PUTFIELD的数据来源不是方法调用返回，或方法的参数1类型，与putfield指令对应的字段的类型相同，不满足set方法
            return false;
        }

        VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) valueVariableDataSource;
        BaseElement objectElement = variableDataSourceMethodCallReturn.getObjectElement();
        if (JavaCGElementUtil.checkElementDataSourceGetArg(objectElement)) {
            // 方法调用的被调用对象中包含方法的参数，满足set方法
            return true;
        }

        List<BaseElement> argumentList = variableDataSourceMethodCallReturn.getArgElementList();
        for (BaseElement argumentElement : argumentList) {
            if (JavaCGElementUtil.checkElementDataSourceGetArg(argumentElement)) {
                // 方法调用的被调用参数中包含方法的参数，满足set方法
                return true;
            }
        }
        return false;
    }

    // 为静态代码块处理PUTSTATIC指令
    private void handlePutStatic4Clinit(PutStaticParseResult putStaticParseResult) {
        if (!callerClassName.equals(putStaticParseResult.getClassName())) {
            // 不是对当前类执行PUTSTATIC，不处理
            return;
        }

        List<InvokeInstructionPosAndCallee> invokeInstructionPosAndCalleeList = sfFieldInvokeInstructionMap.get(putStaticParseResult.getFieldName());
        if (invokeInstructionPosAndCalleeList == null) {
            // 不是static、final变量，不处理
            return;
        }

        BaseElement value = putStaticParseResult.getValue();
        if (!(value instanceof VariableElement)) {
            return;
        }

        VariableElement valueVariableElement = (VariableElement) value;
        AbstractVariableDataSource valueVariableDataSource = valueVariableElement.getVariableDataSource();
        if (!(valueVariableDataSource instanceof VariableDataSourceMethodCallReturn)) {
            return;
        }
        // PUTSTATIC的数据来源是方法调用返回
        VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) valueVariableDataSource;
        InvokeInstructionPosAndCallee invokeInstructionPosAndCallee = new InvokeInstructionPosAndCallee(variableDataSourceMethodCallReturn.getInvokeInstructionPosition(),
                variableDataSourceMethodCallReturn.getCalleeClassName(), variableDataSourceMethodCallReturn.getCalleeMethodName());
        invokeInstructionPosAndCalleeList.add(invokeInstructionPosAndCallee);
    }

    // 处理方法调用指令
    private void handleInvokeInstruction(MethodCallParseResult methodCallParseResult) {
        InvokeInstruction invokeInstruction = (InvokeInstruction) ih.getInstruction();
        // 添加方法调用可能的信息
        if (methodCallPossibleInfoMap == null) {
            methodCallPossibleInfoMap = new HashMap<>(10);
        }
        MethodCallPossibleInfo methodCallPossibleInfo = methodCallPossibleInfoMap.computeIfAbsent(ih.getPosition(), k -> new MethodCallPossibleInfo());

        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokeInstruction, cpg);
        // 获取被调用类名
        String calleeClassName = calleeMethodInfo.getClassName();
        if (JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return;
        }

        // 获取被调用方法
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArgTypes = calleeMethodInfo.getMethodArgumentTypes();
        logger.debug("被调用方法: {}", JavaCGClassMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeArgTypes));

        BaseElement objectElement = methodCallParseResult.getObjectElement();
        // 处理被调用对象类型
        methodCallPossibleInfo.setObjTypeEnum(JavaCGElementUtil.getCalleeObjTypeEnum(objectElement));

        // 处理被调用对象
        if (!JavaCGClassMethodUtil.isInitMethod(callerMethodName) ||
                !JavaCGClassMethodUtil.isObjectClass(calleeClassName) ||
                !JavaCGClassMethodUtil.isInitMethod(calleeMethodName)) {
            // 若是构造函数中调用java.lang.Object的构造函数，则不处理
            methodCallPossibleInfo.addPossibleInfo4Object(objectElement, calleeClassName);
        }

        // 处理参数，序号从0开始
        List<BaseElement> argElementList = methodCallParseResult.getArgumentList();
        for (int i = 0; i < argElementList.size(); i++) {
            BaseElement argElement = argElementList.get(i);
            methodCallPossibleInfo.addPossibleInfo4Args(i, argElement, calleeArgTypes[i].toString());
        }

//        logger.debug("方法调用: " + JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih) + " (" + getSourceLine() + ")" +
//                "\n被调用对象与参数: " + methodCallParseResult);

        // 处理get/set方法关联的字段关系
        recordGetSetMethodFieldRelationship(invokeInstruction, calleeClassName, calleeMethodName, calleeArgTypes, argElementList);
    }

    // 处理athrow指令
    private void handleAThrowInstruction(AThrowParseResult aThrowParseResult) {
        if (aThrowParseResult instanceof AThrowNullParseResult) {
            // throw null写法对应的情况
            return;
        }

        int position = ih.getPosition();
        if (methodThrowPossibleInfoMap == null) {
            methodThrowPossibleInfoMap = new HashMap<>(2);
        }
        ThrowInfoList throwInfoList = methodThrowPossibleInfoMap.computeIfAbsent(position, k -> new ThrowInfoList());
        VariableElement throwElement = aThrowParseResult.getThrowElement();
        Integer catchExceptionStartPosition = throwElement.getCatchExceptionStartPosition();
        if (catchExceptionStartPosition != null) {
            // 当前抛出的异常是catch的异常对象
            String catchExceptionVariableName = "";
            if (throwElement instanceof LocalVariableElement) {
                catchExceptionVariableName = ((LocalVariableElement) throwElement).getName();
            }
            throwInfoList.addThrowInfo(new ThrowInfo(throwElement.getType(), JavaCGConstants.FILE_KEY_THROW_TYPE_CATCH_EXCEPTION, catchExceptionStartPosition,
                    catchExceptionVariableName, null));
            return;
        }
        AbstractVariableDataSource dataSource = throwElement.getVariableDataSource();
        if (dataSource instanceof VariableDataSourceMethodCallReturn) {
            // 当前抛出的异常是方法调用的返回值
            VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) dataSource;
            if (!JavaCGUtil.checkSkipClass(variableDataSourceMethodCallReturn.getCalleeClassName(), javaCGConfInfo.getNeedHandlePackageSet())) {
                // 仅当被调用类需要处理时才执行
                int invokeInstructionPosition = variableDataSourceMethodCallReturn.getInvokeInstructionPosition();
                throwInfoList.addThrowInfo(new ThrowInfo(throwElement.getType(), JavaCGConstants.FILE_KEY_THROW_TYPE_METHOD_CALL_RETURN, null, null, invokeInstructionPosition));
            }
            return;
        }
        // 当前抛出的异常情况未知
        throwInfoList.addThrowInfo(new ThrowInfo(throwElement.getType(), JavaCGConstants.FILE_KEY_THROW_TYPE_UNKNOWN, null, null, null));
    }

    // 处理get/set方法关联的字段关系
    private void recordGetSetMethodFieldRelationship(InvokeInstruction invokeInstruction, String calleeClassName, String calleeMethodName, Type[] calleeArgTypes,
                                                     List<BaseElement> argElementList) {
        if (!analyseFieldRelationshipFlag ||
                invokeInstruction.getOpcode() == Const.INVOKESTATIC ||
                !JavaCGClassMethodUtil.matchesSetMethod(calleeMethodName) ||
                calleeArgTypes.length != 1 ||
                argElementList.size() != 1) {
            return;
        }
        // 若需要分析dto的字段之间的关联关系，且被调用方法不是静态方法，且被调用方法以set开头，且方法参数数量为1，且已获取到的方法参数信息数量为1时，进行处理
        BaseElement arg0Element = argElementList.get(0);
        if (!(arg0Element instanceof VariableElement)) {
            return;
        }
        VariableElement arg0VariableElement = (VariableElement) arg0Element;
        AbstractVariableDataSource arg0VariableDataSource = arg0VariableElement.getVariableDataSource();
        // 当被调用方法的参数1的数据来源为方法调用时，尝试直接获取对应的get方法
        if (getGetMethodFromMethodReturnDirectly(arg0VariableDataSource, calleeClassName, calleeMethodName, false)) {
            return;
        }
        // 使用参数1的数据来源没有获取到对应的get方法
        AbstractVariableDataSource arg0VariableDataSourceEQC = arg0VariableElement.getVariableDataSourceEQC();
        if (arg0VariableDataSourceEQC == null) {
            return;
        }
        // 尝试使用参数1的等值转换前的数据来源获取
        getGetMethodFromMethodReturnDirectly(arg0VariableDataSourceEQC, calleeClassName, calleeMethodName, true);
    }

    /**
     * 当被调用方法的参数1的数据来源为方法调用时，尝试直接获取对应的get方法
     *
     * @param arg0VariableDataSource
     * @param setClassName
     * @param setMethodName
     * @param equivalentConversions
     * @return true: 有记录直接赋值的字段关系 false: 未记录直接赋值的字段关系
     */
    private boolean getGetMethodFromMethodReturnDirectly(AbstractVariableDataSource arg0VariableDataSource, String setClassName, String setMethodName,
                                                         boolean equivalentConversions) {
        if (!(arg0VariableDataSource instanceof VariableDataSourceMethodCallReturn)) {
            return false;
        }
        // 被调用方法的参数1的数据来源为方法调用
        VariableDataSourceMethodCallReturn arg0VariableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) arg0VariableDataSource;
        if (!JavaCGElementUtil.checkDataSourceMethodReturnGetMethod(arg0VariableDataSourceMethodCallReturn)) {
            return false;
        }

        String getClassName = arg0VariableDataSourceMethodCallReturn.getCalleeClassName();
        if (JavaCGUtil.checkSkipClass(getClassName, javaCGConfInfo.getNeedHandlePackageSet()) ||
                JavaCGUtil.checkSkipClass(setClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            // get或set方法对应的类需要跳过，不处理
            return false;
        }

        int position = ih.getPosition();
        // 若被调用方法的参数1的数据来源的方法调用可能是dto的get方法则处理
        Set<Integer> getSetMethodCallSet = getSetMethodCallMap.computeIfAbsent(position, k -> new HashSet<>());
        // 记录通过get/set方法关联的字段关系时，避免重复添加
        if (getSetMethodCallSet.add(arg0VariableDataSourceMethodCallReturn.getInvokeInstructionPosition())) {
            GetSetFieldRelationship getSetFieldRelationship = new GetSetFieldRelationship();
            getSetFieldRelationship.setRecordId(fieldRelationshipCounter.addAndGet());
            getSetFieldRelationship.setGetInvokeInstructionPosition(arg0VariableDataSourceMethodCallReturn.getInvokeInstructionPosition());
            getSetFieldRelationship.setSetInvokeInstructionPosition(position);
            getSetFieldRelationship.setCallerLineNumber(getSourceLine());
            getSetFieldRelationship.setGetClassName(getClassName);
            getSetFieldRelationship.setGetMethodName(arg0VariableDataSourceMethodCallReturn.getCalleeMethodName());
            getSetFieldRelationship.setSetClassName(setClassName);
            getSetFieldRelationship.setSetMethodName(setMethodName);
            getSetFieldRelationship.setValid(JavaCGYesNoEnum.YES.getStrValue());
            getSetFieldRelationship.setType(equivalentConversions ? JavaCGFieldRelationshipTypeEnum.FRTE_DIRECTLY_EQUIVALENT_CONVERSION :
                    JavaCGFieldRelationshipTypeEnum.FRTE_DIRECTLY);
            getSetFieldRelationshipList.add(getSetFieldRelationship);
        }
        return true;
    }

    // 处理带返回值的return类指令，记录对应的返回类型
    private void handleReturnInstructionForType(ReturnParseResult returnParseResult) {
        BaseElement returnInfo = returnParseResult.getReturnElement();
        // 记录对应的返回类型
        if (returnPossibleInfoList == null) {
            returnPossibleInfoList = new ArrayList<>();
            returnPossibleInfoList.add(returnInfo);
            return;
        }

        for (BaseElement baseElement : returnPossibleInfoList) {
            if (JavaCGElementUtil.compare(baseElement, returnInfo)) {
                return;
            }
        }
        returnPossibleInfoList.add(returnInfo);
    }

    // 处理带返回值的return类指令，记录对应的方法参数序号或方法调用指令位置
    private void handleReturnInstructionForArgMethodCall(ReturnParseResult returnParseResult) {
        BaseElement returnInfo = returnParseResult.getReturnElement();
        if (!(returnInfo instanceof VariableElement)) {
            return;
        }
        VariableElement returnVariableElement = (VariableElement) returnInfo;
        // 处理变量的数据来源
        AbstractVariableDataSource variableDataSource = returnVariableElement.getVariableDataSource();
        if (variableDataSource instanceof VariableDataSourceMethodArg) {
            // 方法返回的数据来源是方法参数
            VariableDataSourceMethodArg variableDataSourceMethodArg = (VariableDataSourceMethodArg) variableDataSource;
            if (!methodReturnArgSeqList.contains(variableDataSourceMethodArg.getArgSeq())) {
                // 当返回值的数据来源为方法参数时，记录对应的方法参数序号，避免重复添加同一个方法参数序号
                methodReturnArgSeqList.add(variableDataSourceMethodArg.getArgSeq());
            }
        } else if (variableDataSource instanceof VariableDataSourceMethodCallReturn) {
            // 方法返回的数据来源是方法调用
            VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) variableDataSource;
            if (!JavaCGUtil.checkSkipClass(variableDataSourceMethodCallReturn.getCalleeClassName(), javaCGConfInfo.getNeedHandlePackageSet()) &&
                    !methodReturnPositionList.contains(variableDataSourceMethodCallReturn.getInvokeInstructionPosition())) {
                // 假如被调用方法的类不需要忽略，则记录当前的方法调用指令位置
                // 当返回值的数据来源为方法调用时，记录对应的指令位置，避免重复添加同一个方法的返回结果
                methodReturnPositionList.add(variableDataSourceMethodCallReturn.getInvokeInstructionPosition());
            }
        }

        // 处理变量等值转换前的数据来源
        AbstractVariableDataSource variableDataSourceEQC = returnVariableElement.getVariableDataSourceEQC();
        if (variableDataSourceEQC instanceof VariableDataSourceMethodArg) {
            // 方法返回的数据来源是方法参数
            VariableDataSourceMethodArg variableDataSourceMethodArgEQC = (VariableDataSourceMethodArg) variableDataSourceEQC;
            if (!methodReturnArgSeqEQCList.contains(variableDataSourceMethodArgEQC.getArgSeq())) {
                // 当返回值的数据来源为方法参数时，记录对应的方法参数序号，避免重复添加同一个方法参数序号
                methodReturnArgSeqEQCList.add(variableDataSourceMethodArgEQC.getArgSeq());
            }
        } else if (variableDataSourceEQC instanceof VariableDataSourceMethodCallReturn) {
            // 方法返回的数据来源是方法调用
            VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturnEQC = (VariableDataSourceMethodCallReturn) variableDataSourceEQC;
            if (!JavaCGUtil.checkSkipClass(variableDataSourceMethodCallReturnEQC.getCalleeClassName(), javaCGConfInfo.getNeedHandlePackageSet()) &&
                    !methodReturnPositionEQCList.contains(variableDataSourceMethodCallReturnEQC.getInvokeInstructionPosition())) {
                /*
                    假如被调用方法的类不需要忽略，则记录当前的方法调用指令位置
                    当返回值的数据来源为方法调用时，记录对应的指令位置，避免重复添加同一个方法的返回结果
                 */
                methodReturnPositionEQCList.add(variableDataSourceMethodCallReturnEQC.getInvokeInstructionPosition());
            }
        }
    }

    // 处理带返回值的return类指令，处理可能的Get方法
    private void handleReturnInstruction4GetMethod(ReturnParseResult returnParseResult) {
        // 尝试获取返回对象的数据来源
        BaseElement returnElement = returnParseResult.getReturnElement();
        // 使用返回元素，记录可能是get方法所返回的字段所在类的类名及字段名称
        if (recordGetMethodFieldClassAndName(returnElement, false)) {
            // 返回元素为字段
            return;
        }

        if (!(returnElement instanceof VariableElement)) {
            return;
        }

        // 返回元素为变量
        VariableElement returnInfoVariableElement = (VariableElement) returnElement;
        AbstractVariableDataSource variableDataSource = returnInfoVariableElement.getVariableDataSource();
        if (!(variableDataSource instanceof VariableDataSourceMethodCallReturn)) {
            return;
        }

        // 数据来源类型为方法调用
        VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) variableDataSource;
        BaseElement objectElement = variableDataSourceMethodCallReturn.getObjectElement();
        List<BaseElement> argumentList = variableDataSourceMethodCallReturn.getArgElementList();

        // 使用数据来源方法调用的被调用对象，记录可能是get方法所返回的字段所在类的类名及字段名称
        recordGetMethodFieldClassAndName(objectElement, true);
        // 使用数据来源方法调用的被调用对象，记录可能是get方法所返回的字段所在类的类名及字段名称
        if (!JavaCGUtil.isCollectionEmpty(argumentList)) {
            for (BaseElement argumentElement : argumentList) {
                recordGetMethodFieldClassAndName(argumentElement, true);
            }
        }
    }

    /**
     * 记录可能是get方法所返回的字段所在类的类名及字段名称
     *
     * @param baseElement          可能是字段的元素
     * @param fromMethodCallReturn 当前的变量是否对应方法调用的返回
     * @return true: 指定的元素是字段 false: 指定的元素不是字段
     */
    private boolean recordGetMethodFieldClassAndName(BaseElement baseElement, boolean fromMethodCallReturn) {
        if (!(baseElement instanceof FieldElement)) {
            return false;
        }

        FieldElement fieldElement = (FieldElement) baseElement;
        if (fromMethodCallReturn && !methodReturnType.equals(fieldElement.getType())) {
            // 当前的变量对应方法调用的返回，当前方法的返回类型和当前的变量类型不一致，认为不满足get方法
            return false;
        }

        Set<String> getFieldFieldNameSet = returnDataSourceGetFieldMap.computeIfAbsent(fieldElement.getClassName(), k -> new HashSet<>());
        getFieldFieldNameSet.add(fieldElement.getName());
        return true;
    }

    /**
     * 获取方法调用可能的信息
     *
     * @param position 方法调用指令位置
     * @return
     */
    public MethodCallPossibleInfo getMethodCallPossibleInfo(int position) {
        if (methodCallPossibleInfoMap == null) {
            return null;
        }
        return methodCallPossibleInfoMap.get(position);
    }

    /**
     * 获取方法抛出异常可能的信息
     *
     * @param position 方法调用指令位置
     * @return
     */
    public ThrowInfoList getMethodThrowPossibleInfo(int position) {
        if (methodThrowPossibleInfoMap == null) {
            return null;
        }
        return methodThrowPossibleInfoMap.get(position);
    }

    /**
     * 获取方法可能的返回类型列表
     *
     * @return
     */
    public List<String> getReturnPossibleTypeList() {
        if (returnPossibleInfoList == null) {
            return null;
        }

        List<String> returnPossibleTypeList = new ArrayList<>(returnPossibleInfoList.size());
        for (BaseElement baseElement : returnPossibleInfoList) {
            String type = baseElement.getType();
            if (!returnPossibleTypeList.contains(type)) {
                returnPossibleTypeList.add(type);
            }
        }
        return returnPossibleTypeList;
    }

    // 处理get方法
    public void handleGetMethod() throws IOException {
        if (!maybeGetMethod || returnDataSourceGetFieldMap.size() != 1) {
            return;
        }

        /*
            满足以下条件时，处理get方法
                需要分析dto的字段之间的关联关系
                当前方法可能是get方法
                返回对象的数据来源GETFIELD相关Map数量为1
         */
        Set<String> getFieldFieldNameSet = returnDataSourceGetFieldMap.get(JavaCGConstants.THIS);
        if (getFieldFieldNameSet == null || getFieldFieldNameSet.size() != 1) {
            return;
        }

        /*
            满足以下条件时，说明当前方法属于get方法，记录
                返回对象的数据来源GETFIELD相关Map中key为this的Set非空，且数量为1（没有多个）
         */
        for (String getFieldFieldName : getFieldFieldNameSet) {
            // 记录dto的非静态字段集合中涉及的泛型类型
            String fieldCategory = recordFieldGenericsType(getFieldFieldName);

            // 记录get方法
            String fieldClassType = nonStaticFieldNameTypeMap.get(getFieldFieldName);
            if (fieldCategory == null) {
                fieldCategory = JavaCGClassMethodUtil.isCustomType(fieldClassType) ? JavaCGConstants.FILE_KEY_CATEGORY_CUSTOM : JavaCGConstants.FILE_KEY_CATEGORY_JDK;
            }
            String getFullMethod = JavaCGClassMethodUtil.formatFullMethod(callerClassName, callerMethodName, mg.getArgumentTypes());
            JavaCGFileUtil.write2FileWithTab(getMethodWriter, callerClassName, callerMethodName, getFieldFieldName, fieldCategory, fieldClassType, getFullMethod);
        }
    }

    // 处理set方法
    public void handleSetMethod() throws IOException {
        if (!maybeSetMethod || putFieldClassMap.size() != 1) {
            return;
        }

        /*
            满足以下条件时，处理set方法
                需要分析dto的字段之间的关联关系
                当前方法可能是set方法
                PUTFIELD相关Map数量为1
         */
        Set<String> putFieldFieldNameSet = putFieldClassMap.get(JavaCGConstants.THIS);
        if (putFieldFieldNameSet == null || putFieldFieldNameSet.size() != 1) {
            return;
        }

        /*
            满足以下条件时，说明当前方法属于set方法，记录
                PUTFIELD相关Map中key为this的Set非空，且数量为1（没有多个）
         */
        for (String putFieldFieldName : putFieldFieldNameSet) {
            // 记录dto的非静态字段集合中涉及的泛型类型
            String fieldCategory = recordFieldGenericsType(putFieldFieldName);

            // 记录set方法
            String fieldClassType = nonStaticFieldNameTypeMap.get(putFieldFieldName);
            if (fieldCategory == null) {
                fieldCategory = JavaCGClassMethodUtil.isCustomType(fieldClassType) ? JavaCGConstants.FILE_KEY_CATEGORY_CUSTOM : JavaCGConstants.FILE_KEY_CATEGORY_JDK;
            }
            if (recordedSetMethodSet.add(callerMethodName)) {
                // 若当前类的当前set方法未被记录时，才写入文件，避免一个类存在多个同名set方法时重复记录（get方法无参数，不会出现同名）
                String setFullMethod = JavaCGClassMethodUtil.formatFullMethod(callerClassName, callerMethodName, mg.getArgumentTypes());
                JavaCGFileUtil.write2FileWithTab(setMethodWriter, callerClassName, callerMethodName, putFieldFieldName, fieldCategory, fieldClassType, setFullMethod);
            }
        }
    }

    /**
     * 记录dto的非静态字段集合中涉及的泛型类型
     *
     * @param fieldName
     * @return null: 当前字段不涉及集合 非null: 当前字段涉及集合时对应的类型
     * @throws IOException
     */
    private String recordFieldGenericsType(String fieldName) throws IOException {
        String fieldCategory = null;
        List<String> fieldGenericsTypeList = nonStaticFieldNameGenericsTypeMap.get(fieldName);
        if (fieldGenericsTypeList == null) {
            return null;
        }

        // 若当前字段集合中涉及的泛型类型已记录则不再重复记录
        boolean recorded = recordedFieldWithGenericsTypeSet.add(fieldName);
        int seq = 0;
        for (String fieldGenericsType : fieldGenericsTypeList) {
            boolean isCustomType = JavaCGClassMethodUtil.isCustomType(fieldGenericsType);
            if (isCustomType) {
                // 集合的泛型类型，出现了自定义类型
                fieldCategory = JavaCGConstants.FILE_KEY_CATEGORY_GENERICS_CUSTOM;
            }

            if (!recorded) {
                // 当前字段未记录时进行记录
                JavaCGFileUtil.write2FileWithTab(fieldGenericsTypeWriter,
                        callerClassName,
                        fieldName,
                        String.valueOf(seq),
                        (isCustomType ? JavaCGConstants.FILE_KEY_CATEGORY_CUSTOM : JavaCGConstants.FILE_KEY_CATEGORY_JDK),
                        fieldGenericsType);
                seq++;
            }
        }
        if (fieldCategory == null) {
            fieldCategory = JavaCGConstants.FILE_KEY_CATEGORY_GENERICS_JDK;
        }
        return fieldCategory;
    }

    //
    public void setRecordFieldPossibleTypeFlag(boolean recordFieldPossibleTypeFlag) {
        this.recordFieldPossibleTypeFlag = recordFieldPossibleTypeFlag;
    }

    public void setUseFieldPossibleTypeFlag(boolean useFieldPossibleTypeFlag) {
        this.useFieldPossibleTypeFlag = useFieldPossibleTypeFlag;
    }

    public void setAnalyseFieldRelationshipFlag(boolean analyseFieldRelationshipFlag) {
        this.analyseFieldRelationshipFlag = analyseFieldRelationshipFlag;
    }

    public void setOnlyAnalyseReturnTypeFlag(boolean onlyAnalyseReturnTypeFlag) {
        this.onlyAnalyseReturnTypeFlag = onlyAnalyseReturnTypeFlag;
    }
}
