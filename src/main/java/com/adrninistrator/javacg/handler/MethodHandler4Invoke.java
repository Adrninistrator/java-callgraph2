package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGCalleeObjTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.call.MethodCall;
import com.adrninistrator.javacg.dto.call.MethodCallList;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleEntry;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleInfo;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleList;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg.extensions.code_parser.MethodAnnotationParser;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGAnnotationUtil;
import com.adrninistrator.javacg.util.JavaCGBootstrapMethodUtil;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGInstructionUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import com.adrninistrator.javacg.util.JavaCGMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.BootstrapMethod;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantInvokeDynamic;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumber;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.CHECKCAST;
import org.apache.bcel.generic.INVOKEDYNAMIC;
import org.apache.bcel.generic.INVOKEINTERFACE;
import org.apache.bcel.generic.INVOKESPECIAL;
import org.apache.bcel.generic.INVOKESTATIC;
import org.apache.bcel.generic.INVOKEVIRTUAL;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InvokeInstruction;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/8/21
 * @description: 对方法进行处理，用于处理方法调用指令
 */
public class MethodHandler4Invoke extends AbstractMethodHandler {

    private final UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private final String callerClassName;

    private final String callerMethodName;

    private final String callerMethodArgs;

    private final String callerFullMethod;

    // 保存方法之间调用关系
    private final MethodCallList methodCallList;

    /*
        保存方法调用可能的信息
        key
            方法调用ID
        value
            方法调用可能的信息
     */
    private final Map<Integer, MethodCallPossibleInfo> methodCallInfoMap = new HashMap<>(50);

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;

    private ExtensionsManager extensionsManager;

    private AnnotationAttributesFormatterInterface annotationAttributesFormatter;

    private Writer methodCallWriter;
    private Writer lambdaMethodInfoWriter;
    private Writer methodAnnotationWriter;
    private Writer methodLineNumberWriter;
    private Writer methodCallInfoWriter;

    private int lastJarNum;

    // 是否出现方法名+参数类型均相同的方法标记
    private boolean existsSameMethodNameAndArgs;

    // 非静态字段字段所有可能的类型
    private FieldPossibleTypes nonStaticFieldPossibleTypes;

    private ClassAndJarNum classAndJarNum;

    public MethodHandler4Invoke(MethodGen mg,
                                JavaClass javaClass,
                                JavaCGConfInfo javaCGConfInfo,
                                String callerMethodArgs,
                                String callerFullMethod,
                                UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler,
                                JavaCGCounter callIdCounter) {
        super(mg, javaClass, javaCGConfInfo);

        callerClassName = javaClass.getClassName();
        callerMethodName = mg.getName();

        this.callerMethodArgs = callerMethodArgs;
        this.callerFullMethod = callerFullMethod;
        this.useSpringBeanByAnnotationHandler = useSpringBeanByAnnotationHandler;

        methodCallList = new MethodCallList(callIdCounter);
    }

    /**
     * 方法预处理
     *
     * @return false: 方法不需要继续处理 true: 方法需要继续处理
     */
    @Override
    protected boolean preHandleMethod() throws IOException {
        if (!existsSameMethodNameAndArgs) {
            /*
                对于方法名+参数类型相同的方法，不进行以下处理
                1. 避免出现方法上的重复注解
                2. 避免出现方法对应行号信息重复记录
             */
            // 记录方法上的注解信息
            JavaCGAnnotationUtil.writeAnnotationInfo(callerFullMethod,
                    mg.getMethod().getAnnotationEntries(),
                    annotationAttributesFormatter,
                    methodAnnotationWriter);

            // 处理方法的行号信息
            handleLineNumber(callerFullMethod);
        }

        // 处理方法注解，需要在此执行，否则接口方法上的注解不会被处理
        handleMethodAnnotations();

        // 初始化当前处理的指令
        ih = JavaCGInstructionUtil.getFirstInstructionHandle(mg);
        // 若方法中指令为空，不需要再判断方法是否为abstract或native
        return ih != null;
    }

    // 处理方法的行号信息
    private void handleLineNumber(String callerFullMethod) throws IOException {
        if (lineNumberTable == null) {
            return;
        }

        LineNumber[] lineNumbers = lineNumberTable.getLineNumberTable();
        if (lineNumbers == null || lineNumbers.length == 0) {
            return;
        }

        int minLineNumber = lineNumbers[0].getLineNumber();
        int maxLineNumber;
        if (lineNumbers.length == 1) {
            maxLineNumber = minLineNumber;
        } else {
            maxLineNumber = lineNumbers[lineNumbers.length - 1].getLineNumber();
            // 寻找行号中的最大值
            for (LineNumber lineNumber : lineNumbers) {
                int sourceLineNumber = lineNumber.getLineNumber();
                if(maxLineNumber < sourceLineNumber){
                    maxLineNumber = sourceLineNumber;
                }
            }

        }

        // 记录方法起始代码行号
        JavaCGFileUtil.write2FileWithTab(methodLineNumberWriter, callerFullMethod, String.valueOf(minLineNumber), String.valueOf(maxLineNumber));
    }

    @Override
    protected boolean doHandleMethod() throws IOException {
        MethodHandler4TypeAndValue methodHandler4TypeAndValue = null;
        if (parseMethodCallTypeValueFlag) {
            // 获取方法调用指令对应的类型与值
            methodHandler4TypeAndValue = new MethodHandler4TypeAndValue(mg, javaClass, javaCGConfInfo);
            methodHandler4TypeAndValue.setParseMethodCallTypeValueFlag(true);
            methodHandler4TypeAndValue.setRecordReturnPossibleInfoFlag(false);
            if (javaCGConfInfo.isFirstParseInitMethodType()) {
                methodHandler4TypeAndValue.setRecordFieldPossibleTypeFlag(false);
                methodHandler4TypeAndValue.setUseFieldPossibleTypeFlag(true);
                methodHandler4TypeAndValue.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);
            }
            if (!methodHandler4TypeAndValue.handleMethod()) {
                return false;
            }
        }

        // 遍历指令
        do {
            short opCode = ih.getInstruction().getOpcode();
            if (JavaCGInstructionUtil.isMethodInvokeInstruction(opCode)) {
                // 处理方法调用指令
                handleInvokeInstruction(methodHandler4TypeAndValue);
            }
            ih = ih.getNext();
        } while (ih != null);
        return true;
    }

    @Override
    protected boolean lastStep() throws IOException {
        // 处理方法之间调用关系
        for (MethodCall methodCall : methodCallList.getMethodCallList()) {
            String calleeClassJarNum = classAndJarNum.getJarNum(methodCall.getCalleeClassName());
            JavaCGFileUtil.write2FileWithTab(methodCallWriter, methodCall.genCallContent(String.valueOf(lastJarNum), calleeClassJarNum));

            // 处理方法调用可能的信息
            handleMethodCallPossibleInfo(methodCall.getCallId(), methodCallInfoMap.get(methodCall.getCallId()));
        }
        return true;
    }

    // 处理方法注解
    private void handleMethodAnnotations() {
        for (AnnotationEntry annotationEntry : mg.getMethod().getAnnotationEntries()) {
            String annotationClassName = Utility.typeSignatureToString(annotationEntry.getAnnotationType(), false);
            // 判断方法上每个注解是否存在对应的扩展类处理
            MethodAnnotationParser methodAnnotationParser = extensionsManager.getMethodAnnotationParser(annotationClassName);
            if (methodAnnotationParser == null) {
                continue;
            }

            // 使用扩展类处理方法注解
            methodAnnotationParser.parseMethodAnnotation(callerClassName, callerMethodName, callerMethodArgs, methodReturnType, annotationClassName, annotationEntry,
                    methodCallList);
        }
    }

    // 处理方法调用指令
    private void handleInvokeInstruction(MethodHandler4TypeAndValue methodHandler4TypeAndValue) throws IOException {
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("%%% 处理方法调用指令 " + JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih) + " (" + getSourceLine() + ")");
        }

        MethodCallPossibleInfo methodCallPossibleInfo = null;

        if (parseMethodCallTypeValueFlag && methodHandler4TypeAndValue != null) {
            int position = ih.getPosition();
            // 获取方法调用指令对应的类型与值
            methodCallPossibleInfo = methodHandler4TypeAndValue.getMethodCallPossibleInfo(position);
        }

        Instruction invokeInstruction = ih.getInstruction();
        switch (invokeInstruction.getOpcode()) {
            case Const.INVOKEVIRTUAL:
                // 处理INVOKEVIRTUAL指令
                handleINVOKEVIRTUAL((INVOKEVIRTUAL) invokeInstruction, methodCallPossibleInfo);
                break;
            case Const.INVOKEINTERFACE:
                // 处理INVOKEINTERFACE指令
                handleINVOKEINTERFACE((INVOKEINTERFACE) invokeInstruction, methodCallPossibleInfo);
                break;
            case Const.INVOKESPECIAL:
                // 处理INVOKESPECIAL指令
                handleINVOKESPECIAL((INVOKESPECIAL) invokeInstruction, methodCallPossibleInfo);
                break;
            case Const.INVOKESTATIC:
                // 处理INVOKESTATIC指令
                handleINVOKESTATIC((INVOKESTATIC) invokeInstruction, methodCallPossibleInfo);
                break;
            case Const.INVOKEDYNAMIC:
                // 处理INVOKEDYNAMIC指令
                handleINVOKEDYNAMIC((INVOKEDYNAMIC) invokeInstruction);
                break;
            default:
                System.err.println("不会执行到此: " + invokeInstruction.getOpcode());
                break;
        }
    }

    // 处理INVOKEVIRTUAL指令
    private void handleINVOKEVIRTUAL(INVOKEVIRTUAL invokevirtual, MethodCallPossibleInfo methodCallPossibleInfo) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokevirtual, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        // 记录线程相关的方法调用，Thread子类
        addMethodCall4ThreadStart(calleeClassName, calleeMethodName, calleeArguments);

        if (methodCallPossibleInfo == null) {
            // 记录方法调用信息
            addCommonMethodCall(invokevirtual, JavaCGCallTypeEnum.CTE_RAW_INVOKE_VIRTUAL, null, calleeClassName, calleeMethodName, calleeArguments, null, null);
            return;
        }

        // 处理被调用类型可变的调用
        handleChangeableCalleeType(invokevirtual, false, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInfo);
    }

    // 处理INVOKEINTERFACE指令
    private void handleINVOKEINTERFACE(INVOKEINTERFACE invokeinterface, MethodCallPossibleInfo methodCallPossibleInfo) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokeinterface, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        if (methodCallPossibleInfo == null) {
            // 记录方法调用信息
            addCommonMethodCall(invokeinterface, JavaCGCallTypeEnum.CTE_RAW_INVOKE_INTERFACE, null, calleeClassName, calleeMethodName, calleeArguments, null, null);
            return;
        }

        // 处理被调用类型可变的调用
        handleChangeableCalleeType(invokeinterface, true, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInfo);
    }

    // 处理INVOKESPECIAL指令
    private void handleINVOKESPECIAL(INVOKESPECIAL invokespecial, MethodCallPossibleInfo methodCallPossibleInfo) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokespecial, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        // 记录线程相关的方法调用，Runnable、Callable实现类
        boolean skipRawMethodCall = addMethodCall4SpecialInit(calleeClassName, calleeMethodName, calleeArguments);
        if (skipRawMethodCall) {
            return;
        }

        JavaCGCallTypeEnum callTypeEnum = JavaCGCallTypeEnum.CTE_RAW_INVOKE_SPECIAL;
        if (!JavaCGCommonNameConstants.METHOD_NAME_INIT.equals(calleeMethodName)
                && calleeClassName.equals(javaClass.getSuperclassName())
                && methodCallPossibleInfo != null && JavaCGCalleeObjTypeEnum.COTE_THIS == methodCallPossibleInfo.getObjTypeEnum()) {
            /*
                满足以下所有条件时，将方法调用类型修改为代表super.方法调用：
                - 方法调用指令为INVOKESPECIAL
                - 被调用方法不是<init>
                - 被调用类为调用类的父类
                - 被调用对象是this
             */
            callTypeEnum = JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER_SPECIAL;
        }

        // 记录方法调用信息
        addCommonMethodCallWithInfo(invokespecial, callTypeEnum, null, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInfo);
    }

    // 处理INVOKESTATIC指令
    private void handleINVOKESTATIC(INVOKESTATIC invokestatic, MethodCallPossibleInfo methodCallPossibleInfo) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokestatic, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        // 记录方法调用信息
        addCommonMethodCallWithInfo(invokestatic, JavaCGCallTypeEnum.CTE_RAW_INVOKE_STATIC, null, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInfo);
    }

    // 处理INVOKEDYNAMIC指令
    private void handleINVOKEDYNAMIC(INVOKEDYNAMIC invokedynamic) throws IOException {
        // getReferenceType()方法获取到的类型为java.lang.Object
        String calleeClassName = invokedynamic.getType(cpg).toString();
        String calleeMethodName = invokedynamic.getMethodName(cpg);
        Type[] calleeArguments = invokedynamic.getArgumentTypes(cpg);

        // 记录INVOKEDYNAMIC指令对应的方法调用信息
        addCommonMethodCallWithInfo(invokedynamic, JavaCGCallTypeEnum.CTE_RAW_INVOKE_DYNAMIC, null, calleeClassName, calleeMethodName, calleeArguments, null);

        // 判断是否需要为Lambda表达式
        Constant constant = cpg.getConstant(invokedynamic.getIndex());
        if (!(constant instanceof ConstantInvokeDynamic)) {
            return;
        }

        // 处理Lambda表达式
        ConstantInvokeDynamic cid = (ConstantInvokeDynamic) constant;
        // 获得JavaClass中指定下标的BootstrapMethod
        BootstrapMethod bootstrapMethod = JavaCGBootstrapMethodUtil.getBootstrapMethod(javaClass, cid.getBootstrapMethodAttrIndex());
        if (bootstrapMethod == null) {
            System.err.println("### 无法找到bootstrapMethod " + callerClassName + " " + cid.getBootstrapMethodAttrIndex());
            return;
        }

        // 获得BootstrapMethod的方法信息
        JavaCGMethodInfo bootstrapMethodInfo = JavaCGBootstrapMethodUtil.getBootstrapMethodInfo(bootstrapMethod, javaClass);
        if (bootstrapMethodInfo == null) {
            System.err.println("### 无法找到bootstrapMethod的方法信息 " + callerClassName + " " + bootstrapMethod);
            return;
        }

        // 记录Lambda表达式实际的方法调用信息
        MethodCall methodCall = addCommonMethodCall(invokedynamic, JavaCGCallTypeEnum.CTE_LAMBDA, null, bootstrapMethodInfo.getClassName(), bootstrapMethodInfo.getMethodName(),
                bootstrapMethodInfo.getMethodArgumentTypes(), bootstrapMethodInfo.getMethodReturnType(), null);
        if (methodCall == null) {
            // 当前方法调用不记录，返回
            return;
        }

        // 处理被调用的Lambda表达式方法信息
        String nextCalleeFullMethod = null;

        InstructionHandle nextIh = ih.getNext();
        while (nextIh != null) {
            Instruction nextInstruction = nextIh.getInstruction();
            short nextOpcode = nextInstruction.getOpcode();
            if (!JavaCGInstructionUtil.isMethodInvokeInstruction(nextOpcode)) {
                // 向后找到非方法调用指令则结束
                break;
            }

            if (JavaCGInstructionUtil.isMethodInvokeInstructionExcludeDynamic(nextOpcode)) {
                // 向后找到非INVOKEDYNAMIC方法调用指令，记录对应的方法
                JavaCGMethodInfo nextCalleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo((InvokeInstruction) nextInstruction, cpg);
                nextCalleeFullMethod = JavaCGMethodUtil.formatFullMethod(nextCalleeMethodInfo);
                break;
            }
            nextIh = nextIh.getNext();
        }

        String calleeFullMethod = JavaCGMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeArguments);
        if (nextCalleeFullMethod == null) {
            // 记录被调用的Lambda表达式方法信息，不包含下一个被调用方法信息
            JavaCGFileUtil.write2FileWithTab(lambdaMethodInfoWriter, String.valueOf(methodCall.getCallId()), calleeFullMethod);
        } else {
            // 记录被调用的Lambda表达式方法信息，包含下一个被调用方法信息
            JavaCGFileUtil.write2FileWithTab(lambdaMethodInfoWriter, String.valueOf(methodCall.getCallId()), calleeFullMethod, nextCalleeFullMethod);
        }
    }

    /**
     * 处理被调用类型可变的调用
     *
     * @param invokeInstruction      方法调用指令
     * @param isInterface            被调用对象是否为接口
     * @param calleeClassName
     * @param calleeMethodName
     * @param arguments
     * @param methodCallPossibleInfo
     */
    private void handleChangeableCalleeType(InvokeInstruction invokeInstruction,
                                            boolean isInterface,
                                            String calleeClassName,
                                            String calleeMethodName,
                                            Type[] arguments,
                                            MethodCallPossibleInfo methodCallPossibleInfo) {
        // 记录已处理过的被调用对象类型
        Set<String> handledCalleeTypeSet = new HashSet<>();

        MethodCallPossibleList methodCallPossibleList4Object = methodCallPossibleInfo.getPossibleInfo4Object();
        // 处理Spring Bean相关的被调用对象类型，外层已判断methodCallPossibleInfo非空
        if (useSpringBeanByAnnotationHandler.hasUseSpringBean() && methodCallPossibleList4Object.hasNonStaticField()) {
            // 涉及Spring Bean，获取被调用对象可能的非静态字段名
            for (MethodCallPossibleEntry methodCallPossibleEntry : methodCallPossibleList4Object.getMethodCallPossibleEntryList()) {
                FieldTypeAndName nonStaticField = methodCallPossibleEntry.getNonStaticField();
                if (nonStaticField == null) {
                    continue;
                }
                // 获取指定类指定字段对应的Spring Bean类型
                List<String> springBeanFieldTypeList = useSpringBeanByAnnotationHandler.getSpringBeanTypeList(callerClassName, nonStaticField.getFieldName());
                if (JavaCGUtil.isCollectionEmpty(springBeanFieldTypeList)) {
                    continue;
                }
                for (String springBeanFieldType : springBeanFieldTypeList) {
                    // 尝试添加方法调用信息，使用对应的被调用类型
                    tryAddMethodCallWithType(invokeInstruction, isInterface, true, handledCalleeTypeSet, calleeClassName, springBeanFieldType, calleeMethodName, arguments,
                            methodCallPossibleInfo);
                }
            }
        }

        // 处理一般的被调用对象类型
        if (methodCallPossibleList4Object.hasType()) {
            for (MethodCallPossibleEntry methodCallPossibleEntry : methodCallPossibleList4Object.getMethodCallPossibleEntryList()) {
                String type = methodCallPossibleEntry.getType();
                if (type != null) {
                    // 尝试添加方法调用信息，使用对应的被调用类型
                    tryAddMethodCallWithType(invokeInstruction, isInterface, false, handledCalleeTypeSet, calleeClassName, type, calleeMethodName, arguments,
                            methodCallPossibleInfo);
                }
            }
        }

        if (handledCalleeTypeSet.isEmpty()) {
            // 未添加与方法调用指令中被调用类不同类型的调用信息，使用方法调用指令中被调用类进行添加
            // 获取实际的被调用类型
            JavaCGCallTypeEnum callTypeEnum = isInterface ? JavaCGCallTypeEnum.CTE_RAW_INVOKE_INTERFACE : JavaCGCallTypeEnum.CTE_RAW_INVOKE_VIRTUAL;

            // 记录方法调用信息
            addCommonMethodCallWithInfo(invokeInstruction, callTypeEnum, null, calleeClassName, calleeMethodName, arguments, methodCallPossibleInfo);
        }
    }

    /**
     * 获取实际的被调用类型
     *
     * @param isInterface  被调用对象是否为接口
     * @param isSpringBean 被调用对象是否为Spring Bean
     * @return
     */
    private JavaCGCallTypeEnum chooseActualCallType(boolean isInterface, boolean isSpringBean) {
        // 有替换被调用对象的类型
        if (isSpringBean) {
            return isInterface ? JavaCGCallTypeEnum.CTE_SPRING_BEAN_ACTUAL_INTERFACE : JavaCGCallTypeEnum.CTE_SPRING_BEAN_ACTUAL_CLASS;
        }
        return isInterface ? JavaCGCallTypeEnum.CTE_ACTUAL_INTERFACE : JavaCGCallTypeEnum.CTE_ACTUAL_CLASS;
    }

    /**
     * 尝试添加方法调用信息，使用对应的被调用类型
     *
     * @param invokeInstruction
     * @param isInterface
     * @param isSpringBean
     * @param handledCalleeTypeSet
     * @param calleeClassName
     * @param calleeTypeRuntime
     * @param calleeMethodName
     * @param arguments
     * @param methodCallPossibleInfo
     */
    private void tryAddMethodCallWithType(InvokeInstruction invokeInstruction,
                                          boolean isInterface,
                                          boolean isSpringBean,
                                          Set<String> handledCalleeTypeSet,
                                          String calleeClassName,
                                          String calleeTypeRuntime,
                                          String calleeMethodName,
                                          Type[] arguments,
                                          MethodCallPossibleInfo methodCallPossibleInfo) {
        if (handledCalleeTypeSet.contains(calleeTypeRuntime) ||
                StringUtils.equals(calleeClassName, calleeTypeRuntime) ||
                JavaCGByteCodeUtil.isNullType(calleeTypeRuntime) ||
                JavaCGUtil.isObjectClass(calleeTypeRuntime)) {
                /*
                    以下情况不处理：
                        已处理过的被调用类型
                        被调用类型与方法调用指令中被调用类相同
                        被调用类型为null
                        被调用类型为Object
                 */
            return;
        }

        // 获取实际的被调用类型
        JavaCGCallTypeEnum callTypeEnum = chooseActualCallType(isInterface, isSpringBean);

        // 记录方法调用信息
        addCommonMethodCallWithInfo(invokeInstruction, callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, arguments, methodCallPossibleInfo);
        handledCalleeTypeSet.add(calleeTypeRuntime);
    }

    /**
     * 记录方法调用信息，尝试记录处理可能的信息
     *
     * @param invokeInstruction      方法调用指令
     * @param callTypeEnum           调用类型
     * @param calleeTypeRuntime      运行时的被调用类型
     * @param calleeClassName        方法调用指令中的被调用类名
     * @param calleeMethodName       被调用方法名
     * @param arguments              被调用方法参数
     * @param methodCallPossibleInfo 方法调用可能的信息
     */
    private void addCommonMethodCallWithInfo(InvokeInstruction invokeInstruction,
                                             JavaCGCallTypeEnum callTypeEnum,
                                             String calleeTypeRuntime,
                                             String calleeClassName,
                                             String calleeMethodName,
                                             Type[] arguments,
                                             MethodCallPossibleInfo methodCallPossibleInfo) {
        if (methodCallPossibleInfo == null) {
            addCommonMethodCall(invokeInstruction, callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, arguments, null, null);
            return;
        }

        MethodCall methodCall = addCommonMethodCall(invokeInstruction, callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, arguments, null,
                methodCallPossibleInfo.getObjTypeEnum());
        if (methodCall == null) {
            return;
        }

        methodCallInfoMap.put(methodCall.getCallId(), methodCallPossibleInfo);
    }

    /**
     * 记录方法调用信息
     *
     * @param invokeInstruction 方法调用指令
     * @param callTypeEnum      调用类型
     * @param calleeTypeRuntime 运行时的被调用类型
     * @param calleeClassName   方法调用指令中的被调用类名
     * @param calleeMethodName  被调用方法名
     * @param calleeArgTypes    被调用方法参数
     * @param calleeReturnType  被调用方法返回类型
     * @param objTypeEnum       被调用对象类型
     */
    private MethodCall addCommonMethodCall(InvokeInstruction invokeInstruction,
                                           JavaCGCallTypeEnum callTypeEnum,
                                           String calleeTypeRuntime,
                                           String calleeClassName,
                                           String calleeMethodName,
                                           Type[] calleeArgTypes,
                                           Type calleeReturnType,
                                           JavaCGCalleeObjTypeEnum objTypeEnum) {
        if (JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return null;
        }

        String rawReturnType = "";
        String actualReturnType = "";
        if (invokeInstruction != null) {
            // 传入的方法调用指令非空，处理方法返回类型
            // 获取方法原始返回类型
            Type returnType = invokeInstruction.getReturnType(cpg);
            rawReturnType = returnType.toString();
            if (Type.VOID != returnType) {
                // 获取方法实际返回类型
                InstructionHandle nextIh = ih.getNext();
                if (nextIh != null) {
                    Instruction nextI = nextIh.getInstruction();
                    if (nextI instanceof CHECKCAST) {
                        CHECKCAST checkcast = (CHECKCAST) nextI;
                        actualReturnType = checkcast.getType(cpg).toString();
                    }
                }
            }
        }

        // 若运行时的被调用类型非空则使用，若为空则使用方法调用指令中的被调用类名
        // 假如指定的返回类型calleeReturnType非空则使用，若为空则使用通过方法调用指令获取的方法返回类型
        MethodCall methodCall = new MethodCall(
                callerClassName,
                callerMethodName,
                callerMethodArgs,
                methodReturnType,
                callTypeEnum,
                (calleeTypeRuntime != null ? calleeTypeRuntime : calleeClassName),
                calleeMethodName,
                JavaCGMethodUtil.getArgListStr(calleeArgTypes),
                getSourceLine(),
                objTypeEnum,
                calleeReturnType != null ? calleeReturnType.toString() : rawReturnType,
                actualReturnType
        );
        methodCallList.addMethodCall(methodCall);

        return methodCall;
    }

    // 添加其他方法调用关系
    private void addOtherMethodCall(String callerClassName,
                                    String callerMethodName,
                                    String callerMethodArgs,
                                    String callerReturnType,
                                    JavaCGCallTypeEnum methodCallType,
                                    String calleeClassName,
                                    String calleeMethodName,
                                    String calleeMethodArgs,
                                    String calleeReturnType,
                                    int callerSourceLine) {
        if (JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return;
        }

        MethodCall methodCall = new MethodCall(
                callerClassName,
                callerMethodName,
                callerMethodArgs,
                callerReturnType,
                methodCallType,
                calleeClassName,
                calleeMethodName,
                calleeMethodArgs,
                callerSourceLine,
                null,
                calleeReturnType,
                null
        );
        methodCallList.addMethodCall(methodCall);
    }

    /**
     * 记录特殊的构造函数调用，包括Runnable、Callable实现类、TransactionTemplate相关的类等
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @param arguments
     * @return true: 不记录原始的方法调用类型，false: 记录原始的方法调用类型
     */
    private boolean addMethodCall4SpecialInit(String calleeClassName, String calleeMethodName, Type[] arguments) {
        if (!JavaCGUtil.isInitMethod(calleeMethodName)) {
            // 记录原始的方法调用类型
            return false;
        }

        boolean skipRawMethodCall = false;
        String calleeMethodArgs = JavaCGMethodUtil.getArgListStr(arguments);


        // 处理Runnable实现类，run方法返回类型为void
        if (handleSpecialInitMethod(runnableImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_RUNNABLE_INIT_RUN1,
                JavaCGCallTypeEnum.CTE_RUNNABLE_INIT_RUN2, JavaCGCommonNameConstants.METHOD_RUNNABLE_RUN, JavaCGConstants.EMPTY_METHOD_ARGS,
                JavaCGCommonNameConstants.RETURN_TYPE_VOID)) {
            skipRawMethodCall = true;
        }

        // 处理Callable实现类，call方法返回类型不固定
        if (handleSpecialInitMethod(callableImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_CALLABLE_INIT_CALL1,
                JavaCGCallTypeEnum.CTE_CALLABLE_INIT_CALL2, JavaCGCommonNameConstants.METHOD_CALLABLE_CALL, JavaCGConstants.EMPTY_METHOD_ARGS, "")) {
            skipRawMethodCall = true;
        }

        // 处理TransactionCallback实现类，doInTransaction方法返回类型不固定
        if (handleSpecialInitMethod(transactionCallbackImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_TX_CALLBACK_INIT_CALL1,
                JavaCGCallTypeEnum.CTE_TX_CALLBACK_INIT_CALL2, JavaCGCommonNameConstants.METHOD_DO_IN_TRANSACTION, JavaCGCommonNameConstants.ARGS_TRANSACTION_STATUS, "")) {
            skipRawMethodCall = true;
        }

        // 处理TransactionCallbackWithoutResult实现类，doInTransactionWithoutResult方法返回类型为void
        if (handleSpecialInitMethod(transactionCallbackWithoutResultChildClassMap, calleeClassName, calleeMethodName, calleeMethodArgs,
                JavaCGCallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL1, JavaCGCallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL2,
                JavaCGCommonNameConstants.METHOD_DO_IN_TRANSACTION_WITHOUT_RESULT, JavaCGCommonNameConstants.ARGS_TRANSACTION_STATUS, JavaCGCommonNameConstants.RETURN_TYPE_VOID)) {
            skipRawMethodCall = true;
        }
        return skipRawMethodCall;
    }

    // 处理<init>方法，需要增加调用其他方法的情况
    private boolean handleSpecialInitMethod(Map<String, Boolean> map,
                                            String calleeClassName,
                                            String calleeMethodName,
                                            String calleeMethodArgs,
                                            JavaCGCallTypeEnum callTypeEnum1,
                                            JavaCGCallTypeEnum callTypeEnum2,
                                            String addedCalleeMethodName,
                                            String addedCalleeMethodArgs,
                                            String addedCalleeReturnType) {
        Boolean recorded = map.get(calleeClassName);
        if (recorded == null) {
            return false;
        }

        // 记录其他方法调用对应类的<init>方法
        addOtherMethodCall(callerClassName, callerMethodName, callerMethodArgs, methodReturnType, callTypeEnum1, calleeClassName, calleeMethodName, calleeMethodArgs,
                JavaCGCommonNameConstants.RETURN_TYPE_VOID, getSourceLine());

        if (Boolean.FALSE.equals(recorded)) {
            // 对应类的<init>方法调用需要增加的方法，<init>方法返回类型为void
            addOtherMethodCall(calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCommonNameConstants.RETURN_TYPE_VOID, callTypeEnum2, calleeClassName,
                    addedCalleeMethodName, addedCalleeMethodArgs, addedCalleeReturnType, JavaCGConstants.DEFAULT_LINE_NUMBER);
            // 避免<init>方法调用相关方法被添加多次
            map.put(calleeClassName, Boolean.TRUE);
        }
        return true;
    }

    /**
     * 记录线程相关的方法调用，Thread子类
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @param arguments
     */
    private void addMethodCall4ThreadStart(String calleeClassName, String calleeMethodName, Type[] arguments) {
        if (!JavaCGCommonNameConstants.METHOD_NAME_START.equals(calleeMethodName) || arguments.length > 0) {
            // 被调用方法不是start()，返回
            return;
        }

        // 处理Thread子类
        if (!Boolean.FALSE.equals(threadChildClassMap.get(calleeClassName))) {
            return;
        }

        String calleeMethodArgs = JavaCGMethodUtil.getArgListStr(arguments);
        // 记录Thread子类的start方法调用run方法（以上Map的value等于FALSE时，代表当前类为Thread的子类，且start()方法调用run()方法未添加过）
        // Thead类的start()、run()方法返回类型都是void
        addOtherMethodCall(calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCommonNameConstants.RETURN_TYPE_VOID, JavaCGCallTypeEnum.CTE_THREAD_START_RUN,
                calleeClassName, "run", JavaCGConstants.EMPTY_METHOD_ARGS, JavaCGCommonNameConstants.RETURN_TYPE_VOID, JavaCGConstants.DEFAULT_LINE_NUMBER);
        // 避免start()方法调用run()方法被添加多次
        threadChildClassMap.put(calleeClassName, Boolean.TRUE);
    }

    // 处理方法调用可能的信息
    private void handleMethodCallPossibleInfo(int methodCallId, MethodCallPossibleInfo methodCallPossibleInfo) throws IOException {
        if (methodCallPossibleInfo == null) {
            return;
        }

        // 记录方法调用可能的信息，被调用对象
        recordMethodCallPossibleInfo(methodCallId, JavaCGConstants.METHOD_CALL_POSSIBLE_INFO_SEQ_OBJECT, methodCallPossibleInfo.getPossibleInfo4Object());

        // 记录方法调用可能的信息，参数
        for (int i = 0; i < methodCallPossibleInfo.getPossibleInfoNum4Args(); i++) {
            recordMethodCallPossibleInfo(methodCallId, JavaCGConstants.METHOD_CALL_POSSIBLE_INFO_SEQ_ARGS_START + i, methodCallPossibleInfo.getPossibleInfo4Args(i));
        }
    }

    // 记录方法调用可能的信息
    private void recordMethodCallPossibleInfo(int methodCallId, int argSeq, MethodCallPossibleList methodCallPossibleList) throws IOException {
        if (methodCallPossibleList == null) {
            return;
        }

        List<MethodCallPossibleEntry> methodCallPossibleEntryList = methodCallPossibleList.getMethodCallPossibleEntryList();
        if (JavaCGUtil.isCollectionEmpty(methodCallPossibleEntryList)) {
            return;
        }

        String arrayElementFlag = JavaCGYesNoEnum.parseStrValue(methodCallPossibleList.isArrayElement());
        StringBuilder stringBuilder = new StringBuilder();
        JavaCGCounter typeCounter = new JavaCGCounter(-1);
        for (int seq = 0; seq < methodCallPossibleEntryList.size(); seq++) {
            MethodCallPossibleEntry methodCallPossibleEntry = methodCallPossibleEntryList.get(seq);
            // 记录方法调用可能的类型，类型的序号可能比以上list的元素数量多，需要每次累计
            recordStringMethodCallPossibleType(stringBuilder, methodCallPossibleEntry, methodCallId, argSeq, typeCounter, arrayElementFlag);

            // 处理方法调用可能的值
            recordStringMethodCallPossibleValue(stringBuilder, methodCallPossibleEntry, methodCallId, argSeq, seq, arrayElementFlag);

            // 处理方法调用可能的被调用静态变量
            recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleEntry.getStaticFieldClassAndFieldName(), methodCallId, argSeq,
                    JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD, seq, arrayElementFlag);

            // 处理被调用对象或参数是静态字段方法返回值的可能信息
            recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleEntry.getStaticFieldMethodCall(), methodCallId, argSeq,
                    JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD_METHOD_CALL, seq, arrayElementFlag);
        }
        JavaCGFileUtil.write2FileNoLF(methodCallInfoWriter, stringBuilder.toString());
    }

    // 记录方法调用可能的类型
    private void recordStringMethodCallPossibleType(StringBuilder stringBuilder, MethodCallPossibleEntry methodCallPossibleEntry, int methodCallId, int argSeq,
                                                    JavaCGCounter typeCounter, String arrayElementFlag) {
        if (useSpringBeanByAnnotationHandler.hasUseSpringBean()) {
            // 涉及Spring Bean，获取被调用对象可能的非静态字段名
            FieldTypeAndName nonStaticField = methodCallPossibleEntry.getNonStaticField();
            if (nonStaticField != null) {
                // 获取指定类指定字段对应的Spring Bean类型
                List<String> springBeanFieldTypeList = useSpringBeanByAnnotationHandler.getSpringBeanTypeList(callerClassName, nonStaticField.getFieldName());
                if (!JavaCGUtil.isCollectionEmpty(springBeanFieldTypeList)) {
                    for (String springBeanFieldType : springBeanFieldTypeList) {
                        // 记录信息
                        recordStringMethodCallPossibleInfo(stringBuilder, springBeanFieldType, methodCallId, argSeq, JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_TYPE,
                                typeCounter.addAndGet(), arrayElementFlag);
                    }
                    return;
                }
            }
        }

        // 处理方法调用可能的类型
        String type = methodCallPossibleEntry.getType();
        if (type != null) {
            recordStringMethodCallPossibleInfo(stringBuilder, type, methodCallId, argSeq, JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_TYPE, typeCounter.addAndGet(),
                    arrayElementFlag);
        }
    }

    // 记录方法调用可能的信息
    private void recordStringMethodCallPossibleInfo(StringBuilder stringBuilder, String data, int methodCallId, int argSeq, String type, int seq, String arrayElementFlag) {
        /*
            文件格式：
                方法调用ID
                参数序号
                值的序号
                类型（类型/值/base64后的值/静态变量）
                是否为数组类型
                值（类型/值/base64后的值/静态变量）
         */
        if (data == null) {
            return;
        }
        stringBuilder.append(methodCallId).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                append(argSeq).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                append(seq).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                append(type).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                append(arrayElementFlag).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                append(data).append(JavaCGConstants.NEW_LINE);
    }

    // 处理方法调用可能的值
    private void recordStringMethodCallPossibleValue(StringBuilder stringBuilder, MethodCallPossibleEntry methodCallPossibleEntry, int methodCallId, int argSeq, int seq,
                                                     String arrayElementFlag) {
        Object value = methodCallPossibleEntry.getValue();
        if (value == null) {
            return;
        }

        String type = JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_VALUE;
        String finalValue;
        if (value instanceof String) {
            String strValue = (String) value;
            // 假如值中包含可能导致文件解析时格式不符合预期的字符，则需要进行base64编码
            if (StringUtils.containsAny(strValue, "\r", "\n", "\t")) {
                finalValue = JavaCGUtil.base64Encode(strValue);
                type = JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE;
            } else {
                finalValue = strValue;
            }
        } else {
            finalValue = value.toString();
        }
        // 记录可能的值
        recordStringMethodCallPossibleInfo(stringBuilder, finalValue, methodCallId, argSeq, type, seq, arrayElementFlag);
    }

    //
    public void setRunnableImplClassMap(Map<String, Boolean> runnableImplClassMap) {
        this.runnableImplClassMap = runnableImplClassMap;
    }

    public void setCallableImplClassMap(Map<String, Boolean> callableImplClassMap) {
        this.callableImplClassMap = callableImplClassMap;
    }

    public void setTransactionCallbackImplClassMap(Map<String, Boolean> transactionCallbackImplClassMap) {
        this.transactionCallbackImplClassMap = transactionCallbackImplClassMap;
    }

    public void setTransactionCallbackWithoutResultChildClassMap(Map<String, Boolean> transactionCallbackWithoutResultChildClassMap) {
        this.transactionCallbackWithoutResultChildClassMap = transactionCallbackWithoutResultChildClassMap;
    }

    public void setThreadChildClassMap(Map<String, Boolean> threadChildClassMap) {
        this.threadChildClassMap = threadChildClassMap;
    }

    public void setExtensionsManager(ExtensionsManager extensionsManager) {
        this.extensionsManager = extensionsManager;
        annotationAttributesFormatter = extensionsManager.getAnnotationAttributesFormatter();
    }

    public void setMethodCallWriter(Writer methodCallWriter) {
        this.methodCallWriter = methodCallWriter;
    }

    public void setLambdaMethodInfoWriter(Writer lambdaMethodInfoWriter) {
        this.lambdaMethodInfoWriter = lambdaMethodInfoWriter;
    }

    public void setMethodAnnotationWriter(Writer methodAnnotationWriter) {
        this.methodAnnotationWriter = methodAnnotationWriter;
    }

    public void setMethodLineNumberWriter(Writer methodLineNumberWriter) {
        this.methodLineNumberWriter = methodLineNumberWriter;
    }

    public void setMethodCallInfoWriter(Writer methodCallInfoWriter) {
        this.methodCallInfoWriter = methodCallInfoWriter;
    }

    public void setLastJarNum(int lastJarNum) {
        this.lastJarNum = lastJarNum;
    }

    public void setExistsSameMethodNameAndArgs(boolean existsSameMethodNameAndArgs) {
        this.existsSameMethodNameAndArgs = existsSameMethodNameAndArgs;
    }

    public void setNonStaticFieldPossibleTypes(FieldPossibleTypes nonStaticFieldPossibleTypes) {
        this.nonStaticFieldPossibleTypes = nonStaticFieldPossibleTypes;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
