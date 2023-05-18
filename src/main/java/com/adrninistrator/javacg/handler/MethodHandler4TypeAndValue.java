package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.enums.JavaCGCalleeObjTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.branch.BranchStackEntry;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleInfo;
import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg.dto.element.variable.VariableElement;
import com.adrninistrator.javacg.dto.exception.ExceptionTargetInfo;
import com.adrninistrator.javacg.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg.dto.frame.FieldInformation;
import com.adrninistrator.javacg.dto.frame.FrameSnapshotEntry;
import com.adrninistrator.javacg.dto.frame.FrameSnapshotsOfIhs;
import com.adrninistrator.javacg.dto.frame.InstructionStep;
import com.adrninistrator.javacg.dto.frame.JavaCGLocalVariables;
import com.adrninistrator.javacg.dto.frame.JavaCGOperandStack;
import com.adrninistrator.javacg.dto.instruction.BaseInstructionParseResult;
import com.adrninistrator.javacg.dto.instruction.MethodCallParseResult;
import com.adrninistrator.javacg.dto.instruction.RetParseResult;
import com.adrninistrator.javacg.dto.instruction.ReturnParseResult;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.util.JavaCGElementUtil;
import com.adrninistrator.javacg.util.JavaCGInstructionUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LocalVariableTable;
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

    // 指令处理类
    private InstructionHandler instructionHandler;

    // 操作数栈
    private JavaCGOperandStack stack;

    // 本地变量
    private JavaCGLocalVariables locals;

    // 非静态变量
    private FieldInformation nonStaticFieldInfo;

    // 静态变量
    private FieldInformation staticFieldInfo;

    // 跳转指令的目标指令位置
    private Set<Integer> jumpTargetIhPositionSet;

    // 用于遍历分支指令的栈
    private ListAsStack<BranchStackEntry> branchStack;

    /*
        分支指令对应的栈桢信息快照
        key
            分支指令位置
        value
            栈桢信息快照
     */
    private Map<Integer, FrameSnapshotEntry> frameSnapshotMap4Branch;

    // 跳转目标指令与对应的栈桢信息快照列表
    private FrameSnapshotsOfIhs frameSnapshotsOfIhs4JumpTargets;

    /*
        异常处理的跳转信息
        key
            Exception table的to指令位置
        value
            Exception table的target指令及异常类型列表
     */
    private Map<Integer, List<ExceptionTargetInfo>> exceptionTargetMap;

    // 异常对应的栈桢信息快照
    private FrameSnapshotsOfIhs frameSnapshotsOfIhs4Exceptions;

    // 指令执行的步骤
    private InstructionStep instructionStep;

    /*
        分支指令对应的执行步骤
        key
            分支指令位置
        value
            指令执行的步骤
     */
    private Map<Integer, InstructionStep> instructionStepMap4Branch;

    /*
        方法调用可能的信息Map
        key
            方法调用指令位置
        value
            方法调用可能的信息
     */
    private Map<Integer, MethodCallPossibleInfo> methodCallPossibleInfoMap;

    // 方法可能的返回信息列表
    private List<BaseElement> returnPossibleInfoList;

    // 需要记录返回对象的可能信息的开关
    private boolean recordReturnPossibleInfoFlag;

    // 解析构造函数以获取非静态字段可能的类型的开关
    private boolean recordFieldPossibleTypeFlag;

    // 使用已获取的构造函数非静态字段可能的类型的开关
    private boolean useFieldPossibleTypeFlag;

    // 非静态字段字段所有可能的类型
    private FieldPossibleTypes nonStaticFieldPossibleTypes;

    public MethodHandler4TypeAndValue(MethodGen mg, JavaClass javaClass, JavaCGConfInfo javaCGConfInfo) {
        super(mg, javaClass, javaCGConfInfo);
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
                Exception table中to的位置
            value
                实际使用的Exception table中to的位置
         */
        Map<Integer, Integer> exceptionTableToMap = new HashMap<>();
        exceptionTargetMap = new HashMap<>();

        for (CodeExceptionGen codeExceptionGen : codeExceptionGens) {
            int exceptionTableToPosition = codeExceptionGen.getEndPC().getPosition();
            if (exceptionTableToPosition == codeExceptionGen.getHandlerPC().getPosition()) {
                int exceptionTableToPrevPosition = codeExceptionGen.getEndPC().getPrev().getPosition();
                if (JavaCGLogUtil.isDebugPrintFlag()) {
                    JavaCGLogUtil.debugPrint("### 修改使用的Exception table的to的位置 " + exceptionTableToPosition + " " + exceptionTableToPrevPosition);
                }
                exceptionTableToMap.put(exceptionTableToPosition, exceptionTableToPrevPosition);
            }
        }

        for (CodeExceptionGen codeExceptionGen : codeExceptionGens) {
            // 处理Exception table的to的位置
            int exceptionTableToPosition = codeExceptionGen.getEndPC().getPosition();
            Integer actualExceptionTableToPosition = exceptionTableToMap.get(exceptionTableToPosition);
            int usedExceptionTableToPosition = (actualExceptionTableToPosition != null ? actualExceptionTableToPosition : exceptionTableToPosition);
            List<ExceptionTargetInfo> exceptionTargetList = exceptionTargetMap.computeIfAbsent(usedExceptionTableToPosition, k -> new ArrayList<>());

            // 处理异常类型
            ObjectType catchType = codeExceptionGen.getCatchType();
            String exceptionType = (catchType != null ? catchType.getClassName() : JavaCGConstantTypeEnum.CONSTTE_NULL.getType());

            // 添加Exception table的target指令及异常类型
            ExceptionTargetInfo exceptionTargetInfo = new ExceptionTargetInfo(codeExceptionGen.getHandlerPC(), exceptionType);
            exceptionTargetList.add(exceptionTargetInfo);
        }

        frameSnapshotsOfIhs4Exceptions = new FrameSnapshotsOfIhs();
    }

    // 判断当前处理的指令是否为Exception table的to指令
    private boolean checkExceptionTo(int position) {
        if (exceptionTargetMap == null) {
            return false;
        }

        return exceptionTargetMap.get(position) != null;
    }

    @Override
    protected boolean doHandleMethod() {
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint(recordFieldPossibleTypeFlag ? "@@@ 预处理构造函数 " : "@@@ 处理方法 " +
                    JavaCGMethodUtil.formatFullMethod(javaClass.getClassName(), mg.getName(), mg.getArgumentTypes()));
        }

        // 初始化需要的对象
        initObjects();

        // 处理正常指令
        handleAllInstruction();

        // 处理异常处理catch中的指令
        handleExceptionCatchInstructions();
        return true;
    }

    // 初始化需要的对象
    private void initObjects() {
        LocalVariableTable localVariableTable = mg.getLocalVariableTable(cpg);
        stack = new JavaCGOperandStack(mg.getMaxStack());
        locals = new JavaCGLocalVariables(mg);
        nonStaticFieldInfo = new FieldInformation();
        staticFieldInfo = new FieldInformation();
        frameSnapshotMap4Branch = new HashMap<>();

        if (JavaCGLogUtil.checkDebugFlag()) {
            instructionStep = new InstructionStep();
            instructionStepMap4Branch = new HashMap<>();
        }
        instructionHandler = new InstructionHandler(mg, localVariableTable, stack, locals, nonStaticFieldInfo, staticFieldInfo);
        instructionHandler.setRecordFieldPossibleTypeFlag(recordFieldPossibleTypeFlag);
        instructionHandler.setUseFieldPossibleTypeFlag(useFieldPossibleTypeFlag);
        instructionHandler.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);
    }

    // 处理所有的指令
    private void handleAllInstruction() {
        while (true) {
            int position = ih.getPosition();
            // 解析当前指令
            if (JavaCGLogUtil.isDebugPrintFlag()) {
                JavaCGLogUtil.debugPrint("### 处理指令 " + JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih) + " (" + getSourceLine() + ")");
            }

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

            if (instructionStep != null) {
                // 记录指令执行步骤
                instructionStep.add(JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih), getSourceLine());
            }

            // 判断当前处理的指令是否为Exception table的to指令
            if (checkExceptionTo(position)) {
                // 当前指令有对应的异常处理，添加信息快照
                if (JavaCGLogUtil.isDebugPrintFlag()) {
                    JavaCGLogUtil.debugPrint("### 异常处理，添加信息快照 " + JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih));
                }
                frameSnapshotsOfIhs4Exceptions.addSnapshot(position, stack, locals, nonStaticFieldInfo, staticFieldInfo);
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
    private boolean handleInstructionsByType(BaseInstructionParseResult instructionParseResult) {
        short opCode = ih.getInstruction().getOpcode();
        if (JavaCGInstructionUtil.isIfInstruction(opCode)) {
            // 处理if类指令
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
            if (recordReturnPossibleInfoFlag && JavaCGInstructionUtil.isReturnWithValueInstruction(opCode)) {
                // 处理带返回值的return类指令
                handleReturnInstruction((ReturnParseResult) instructionParseResult);
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

        if (parseMethodCallTypeValueFlag && JavaCGInstructionUtil.isMethodInvokeInstructionExcludeDynamic(opCode)) {
            // 处理方法调用指令，排除INVOKEDYNAMIC
            handleInvokeInstructionExcludeDynamic((MethodCallParseResult) instructionParseResult);
        }

        // 处理下一条指令
        setIh2Next(ih.getNext(), "next");
        return false;
    }

    // 处理异常处理catch中的指令
    private void handleExceptionCatchInstructions() {
        if (exceptionTargetMap == null) {
            return;
        }

        // Exception table的to指令位置列表
        List<Integer> exceptionToPositionList = new ArrayList<>(exceptionTargetMap.keySet());
        Collections.sort(exceptionToPositionList);

        for (int exceptionToPosition : exceptionToPositionList) {
            if (JavaCGLogUtil.isDebugPrintFlag()) {
                JavaCGLogUtil.debugPrint("### 处理异常处理指令 [" + exceptionToPosition + "]");
            }

            // Exception table的target指令及异常类型列表
            List<ExceptionTargetInfo> exceptionTargetList = exceptionTargetMap.get(exceptionToPosition);
            if (exceptionTargetList == null) {
                continue;
            }

            // 异常对应的栈桢信息快照
            List<FrameSnapshotEntry> exceptionToFrameSnapshotList = frameSnapshotsOfIhs4Exceptions.get(exceptionToPosition);
            if (exceptionToFrameSnapshotList == null) {
                continue;
            }

            for (ExceptionTargetInfo exceptionTargetInfo : exceptionTargetList) {
                // 以下for循环需要使用下标遍历
                for (int i = 0; i < exceptionToFrameSnapshotList.size(); i++) {
                    FrameSnapshotEntry exceptionToFrameSnapshot = exceptionToFrameSnapshotList.get(i);
                    ih = exceptionTargetInfo.getTarget();
                    // 重新设置当前使用的栈桢对应的信息，清空操作数栈并在栈顶放入异常类型
                    resetFrameInfo(exceptionToFrameSnapshot, exceptionTargetInfo.getExceptionType());

                    if (instructionStep != null) {
                        // 清空指令执行的步骤
                        instructionStep.clear();
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
     * @param exceptionType      异常处理的异常类型，非空时需要清空操作数栈并在栈顶放入异常类型
     */
    private void resetFrameInfo(FrameSnapshotEntry frameSnapshotEntry, String exceptionType) {
        if (exceptionType != null) {
            // 清空操作数栈并在栈顶放入异常类型
            stack.clear();
            stack.push(new VariableElement(exceptionType));
        } else {
            stack = frameSnapshotEntry.copyStackSnapshot();
        }
        instructionHandler.setStack(stack);

        locals = frameSnapshotEntry.copyLocalsSnapshot();
        instructionHandler.setLocals(locals);

        nonStaticFieldInfo = frameSnapshotEntry.copyNonStaticFieldInfo();
        instructionHandler.setNonStaticFieldInfo(nonStaticFieldInfo);

        staticFieldInfo = frameSnapshotEntry.copyStaticFieldInfo();
        instructionHandler.setStaticFieldInfo(staticFieldInfo);
    }

    private void setIh2Next(InstructionHandle nextIh, String type) {
        ih = nextIh;
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint(type + " 下一条处理的指令 " + ih.getPosition());
        }
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
        if (!frameSnapshotsOfIhs4JumpTargets.addSnapshot(position, stack, locals, nonStaticFieldInfo, staticFieldInfo)) {
            if (JavaCGLogUtil.isDebugPrintFlag()) {
                JavaCGLogUtil.debugPrint("跳转目标指令[跳过]处理 " + position);
            }
            return true;
        }

        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("跳转目标指令[需要]处理 " + position);
        }
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
        for (int i = 0; i <= branchStack.getHead(); i++) {
            BranchStackEntry tmpBranchStackEntry = branchStack.getElement(i);
            if (ihPosition == tmpBranchStackEntry.getBranchIh().getPosition()) {
                // 当前指令已在栈中，处理用于遍历分支指令的栈中栈顶元素
                return handleBranchStackTopEntry();
            }
        }

        // 用于遍历分支指令的栈入栈
        branchStack.push(branchStackEntry);
        Integer branchStackIndex = branchStack.getHead();

        // 继续处理当前分支指令的第1个目标指令
        setIh2Next(branchStackEntry.getTargetIhList().get(0), "branch");

        // 复制栈桢信息快照，记录与分支指令的关系
        FrameSnapshotEntry frameSnapshotEntry = new FrameSnapshotEntry(stack.copy(), locals.copy(), nonStaticFieldInfo.copy(), staticFieldInfo.copy());
        frameSnapshotMap4Branch.put(branchStackIndex, frameSnapshotEntry);

        // 复制指令执行步骤，记录与分支指令的关系
        if (instructionStep != null) {
            instructionStepMap4Branch.put(branchStackIndex, instructionStep.copy());
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
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("处理用于遍历分支指令的栈中栈顶元素 " + branchPosition);
        }

        Integer branchStackIndex = branchStack.getHead();
        int targetIhIndex = branchStackEntry.getTargetIhIndex();
        targetIhIndex++;

        List<InstructionHandle> targetIhList = branchStackEntry.getTargetIhList();
        if (targetIhIndex >= targetIhList.size()) {
            // 当前分支目标指令已处理完毕

            // 清除分支指令对应的栈桢信息快照
            frameSnapshotMap4Branch.remove(branchStackIndex);

            if (instructionStep != null) {
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

        if (instructionStep != null) {
            // 恢复本地变量到分支指令对应的情况
            instructionStep = instructionStepMap4Branch.get(branchStackIndex).copy();
        }

        branchStackEntry.setTargetIhIndex(targetIhIndex);

        // 指令处理未完毕
        return false;
    }

    // 处理方法调用指令，排除INVOKEDYNAMIC
    private void handleInvokeInstructionExcludeDynamic(MethodCallParseResult methodCallParseResult) {
        InvokeInstruction invokeInstruction = (InvokeInstruction) ih.getInstruction();
        // 添加方法调用可能的信息
        if (methodCallPossibleInfoMap == null) {
            methodCallPossibleInfoMap = new HashMap<>(10);
        }
        MethodCallPossibleInfo methodCallPossibleInfo = methodCallPossibleInfoMap.computeIfAbsent(ih.getPosition(), k -> new MethodCallPossibleInfo());

        // 获取调用方法
        String callerMethodName = mg.getName();
        // 获取被调用类名
        String calleeClassName = invokeInstruction.getReferenceType(cpg).toString();
        // 获取被调用方法
        String calleeMethodName = invokeInstruction.getMethodName(cpg);
        Type[] argTypes = invokeInstruction.getArgumentTypes(cpg);
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("被调用方法: " + JavaCGMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, argTypes));
        }

        BaseElement objectElement = methodCallParseResult.getObjectElement();
        // 处理被调用对象类型
        methodCallPossibleInfo.setObjTypeEnum(getCalleeObjTypeEnum(objectElement));

        // 处理被调用对象
        if (!JavaCGUtil.isInitMethod(callerMethodName) ||
                !JavaCGUtil.isObjectClass(calleeClassName) ||
                !JavaCGUtil.isInitMethod(calleeMethodName)) {
            // 若是构造函数中调用java.lang.Object的构造函数，则不处理
            methodCallPossibleInfo.addPossibleInfo4Object(objectElement, calleeClassName);
        }

        // 处理参数，序号从0开始
        List<BaseElement> argumentList = methodCallParseResult.getArgumentList();
        for (int i = 0; i < argumentList.size(); i++) {
            BaseElement baseElement = argumentList.get(i);
            methodCallPossibleInfo.addPossibleInfo4Args(i, baseElement, argTypes[i].toString());
        }

//        if (JavaCGLogUtil.isDebugPrintFlag()) {
//            JavaCGLogUtil.debugPrint("方法调用: " + JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih) + " (" + getSourceLine() + ")" +
//                    "\n被调用对象与参数: " + methodCallParseResult);
//        }
    }

    // 获取被调用对象类型
    private JavaCGCalleeObjTypeEnum getCalleeObjTypeEnum(BaseElement objectElement) {
        if (objectElement instanceof StaticFieldElement) {
            return JavaCGCalleeObjTypeEnum.COTE_STATIC_FIELD;
        }

        if (objectElement instanceof FieldElement) {
            return JavaCGCalleeObjTypeEnum.COTE_FIELD;
        }

        if (objectElement instanceof LocalVariableElement) {
            LocalVariableElement objLocalVariableElement = (LocalVariableElement) objectElement;
            if (objLocalVariableElement.isThis()) {
                return JavaCGCalleeObjTypeEnum.COTE_THIS;
            }
        }

        if (objectElement instanceof VariableElement) {
            return JavaCGCalleeObjTypeEnum.COTE_VARIABLE;
        }
        return null;
    }

    // 处理return指令
    private void handleReturnInstruction(ReturnParseResult returnParseResult) {
        BaseElement returnInfo = returnParseResult.getReturnInfo();
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

    public void setRecordReturnPossibleInfoFlag(boolean recordReturnPossibleInfoFlag) {
        this.recordReturnPossibleInfoFlag = recordReturnPossibleInfoFlag;
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
