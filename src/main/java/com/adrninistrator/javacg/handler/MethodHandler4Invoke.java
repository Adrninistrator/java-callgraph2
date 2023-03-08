package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGCalleeObjTypeEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.call.MethodCall;
import com.adrninistrator.javacg.dto.call.MethodCallList;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleInfoEntry;
import com.adrninistrator.javacg.dto.call.MethodCallPossibleInformation;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg.dto.field.FieldTypeAndName;
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
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.BootstrapMethod;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantInvokeDynamic;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumber;
import org.apache.bcel.classfile.Utility;
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
import java.util.Collections;
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
    private final Map<Integer, MethodCallPossibleInformation> methodCallInfoMap = new HashMap<>(50);

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
            JavaCGFileUtil.write2FileWithTab(methodCallWriter, methodCall.genCallContent(), String.valueOf(lastJarNum));

            // 处理方法调用可能的信息
            handleMethodCallPossibleInformation(methodCall.getCallId(), methodCallInfoMap.get(methodCall.getCallId()));
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
            methodAnnotationParser.parseMethodAnnotation(callerClassName, callerMethodName, callerMethodArgs, annotationClassName, annotationEntry, methodCallList);
        }
    }

    // 处理方法调用指令
    private void handleInvokeInstruction(MethodHandler4TypeAndValue methodHandler4TypeAndValue) throws IOException {
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("%%% 处理方法调用指令 " + JavaCGInstructionUtil.getInstructionHandlePrintInfo(ih) + " (" + getSourceLine() + ")");
        }

        MethodCallPossibleInformation methodCallPossibleInformation = null;

        if (parseMethodCallTypeValueFlag && methodHandler4TypeAndValue != null) {
            int position = ih.getPosition();
            // 获取方法调用指令对应的类型与值
            methodCallPossibleInformation = methodHandler4TypeAndValue.getMethodCallPossibleInformation(position);
        }

        Instruction invokeInstruction = ih.getInstruction();
        switch (invokeInstruction.getOpcode()) {
            case Const.INVOKEVIRTUAL:
                // 处理INVOKEVIRTUAL指令
                handleINVOKEVIRTUAL((INVOKEVIRTUAL) invokeInstruction, methodCallPossibleInformation);
                break;
            case Const.INVOKEINTERFACE:
                // 处理INVOKEINTERFACE指令
                handleINVOKEINTERFACE((INVOKEINTERFACE) invokeInstruction, methodCallPossibleInformation);
                break;
            case Const.INVOKESPECIAL:
                // 处理INVOKESPECIAL指令
                handleINVOKESPECIAL((INVOKESPECIAL) invokeInstruction, methodCallPossibleInformation);
                break;
            case Const.INVOKESTATIC:
                // 处理INVOKESTATIC指令
                handleINVOKESTATIC((INVOKESTATIC) invokeInstruction, methodCallPossibleInformation);
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
    private void handleINVOKEVIRTUAL(INVOKEVIRTUAL invokevirtual, MethodCallPossibleInformation methodCallPossibleInformation) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokevirtual, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        // 记录线程相关的方法调用，Thread子类
        addMethodCall4ThreadStart(calleeClassName, calleeMethodName, calleeArguments);

        if (methodCallPossibleInformation == null) {
            // 记录方法调用信息
            addCommonMethodCall(JavaCGCallTypeEnum.CTE_RAW_INVOKE_VIRTUAL, null, calleeClassName, calleeMethodName, calleeArguments, null);
            return;
        }

        // 处理被调用类型可变的调用
        handleChangeableCalleeType(false, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInformation);
    }

    // 处理INVOKEINTERFACE指令
    private void handleINVOKEINTERFACE(INVOKEINTERFACE invokeinterface, MethodCallPossibleInformation methodCallPossibleInformation) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokeinterface, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        if (methodCallPossibleInformation == null) {
            // 记录方法调用信息
            addCommonMethodCall(JavaCGCallTypeEnum.CTE_RAW_INVOKE_INTERFACE, null, calleeClassName, calleeMethodName, calleeArguments, null);
            return;
        }

        // 处理被调用类型可变的调用
        handleChangeableCalleeType(true, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInformation);
    }

    // 处理INVOKESPECIAL指令
    private void handleINVOKESPECIAL(INVOKESPECIAL invokespecial, MethodCallPossibleInformation methodCallPossibleInformation) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokespecial, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        // 记录线程相关的方法调用，Runnable、Callable实现类
        boolean skipRawMethodCall = addMethodCall4SpecialInit(calleeClassName, calleeMethodName, calleeArguments);
        if (skipRawMethodCall) {
            return;
        }

        if (methodCallPossibleInformation == null) {
            // 记录方法调用信息
            addCommonMethodCall(JavaCGCallTypeEnum.CTE_RAW_INVOKE_SPECIAL, null, calleeClassName, calleeMethodName, calleeArguments, null);
            return;
        }

        // 记录方法调用信息
        addCommonMethodCallWithInfo(JavaCGCallTypeEnum.CTE_RAW_INVOKE_SPECIAL, null, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInformation);
    }

    // 处理INVOKESTATIC指令
    private void handleINVOKESTATIC(INVOKESTATIC invokestatic, MethodCallPossibleInformation methodCallPossibleInformation) {
        JavaCGMethodInfo calleeMethodInfo = JavaCGInstructionUtil.getCalleeMethodInfo(invokestatic, cpg);
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArguments = calleeMethodInfo.getMethodArgumentTypes();

        if (methodCallPossibleInformation == null) {
            // 记录方法调用信息
            addCommonMethodCall(JavaCGCallTypeEnum.CTE_RAW_INVOKE_STATIC, null, calleeClassName, calleeMethodName, calleeArguments, null);
            return;
        }

        // 记录方法调用信息
        addCommonMethodCallWithInfo(JavaCGCallTypeEnum.CTE_RAW_INVOKE_STATIC, null, calleeClassName, calleeMethodName, calleeArguments, methodCallPossibleInformation);
    }

    // 处理INVOKEDYNAMIC指令
    private void handleINVOKEDYNAMIC(INVOKEDYNAMIC invokedynamic) throws IOException {
        // getReferenceType()方法获取到的类型为java.lang.Object
        String calleeClassName = invokedynamic.getType(cpg).toString();
        String calleeMethodName = invokedynamic.getMethodName(cpg);
        Type[] calleeArguments = invokedynamic.getArgumentTypes(cpg);

        // 记录INVOKEDYNAMIC指令对应的方法调用信息
        addCommonMethodCall(JavaCGCallTypeEnum.CTE_RAW_INVOKE_DYNAMIC, null, calleeClassName, calleeMethodName, calleeArguments, null);

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
        MethodCall methodCall = addCommonMethodCall(JavaCGCallTypeEnum.CTE_LAMBDA, null, bootstrapMethodInfo.getClassName(), bootstrapMethodInfo.getMethodName(),
                bootstrapMethodInfo.getMethodArgumentTypes(), null);
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
                nextCalleeFullMethod = JavaCGByteCodeUtil.formatFullMethod(nextCalleeMethodInfo);
                break;
            }
            nextIh = nextIh.getNext();
        }

        String calleeFullMethod = JavaCGByteCodeUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeArguments);
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
     * @param isInterface                   被调用对象是否为接口
     * @param calleeClassName
     * @param calleeMethodName
     * @param arguments
     * @param methodCallPossibleInformation
     */
    private void handleChangeableCalleeType(boolean isInterface,
                                            String calleeClassName,
                                            String calleeMethodName,
                                            Type[] arguments,
                                            MethodCallPossibleInformation methodCallPossibleInformation) {
        // 记录已处理过的被调用对象类型
        Set<String> handledCalleeTypeSet = new HashSet<>();

        MethodCallPossibleInfoEntry methodCallPossibleInfoEntry4Object = methodCallPossibleInformation.getPossibleInfo4Object();
        // 处理Spring Bean相关的被调用对象类型
        if (javaCGConfInfo.isParseMethodCallTypeValue() && useSpringBeanByAnnotationHandler.hasUseSpringBean()) {
            // 涉及Spring Bean，获取被调用对象可能的非静态字段名
            List<FieldTypeAndName> objectPossibleNonStaticFieldList = methodCallPossibleInfoEntry4Object.getPossibleNonStaticFieldList();
            if (objectPossibleNonStaticFieldList != null) {
                for (FieldTypeAndName fieldTypeAndName : objectPossibleNonStaticFieldList) {
                    // 获取指定类指定字段对应的Spring Bean类型
                    List<String> springBeanFieldTypeList = useSpringBeanByAnnotationHandler.getSpringBeanTypeList(callerClassName, fieldTypeAndName.getFieldName());
                    if (JavaCGUtil.isCollectionEmpty(springBeanFieldTypeList)) {
                        continue;
                    }
                    // 尝试添加方法调用信息，使用对应的被调用类型
                    tryAddMethodCallWithType(isInterface, true, handledCalleeTypeSet, calleeClassName, springBeanFieldTypeList, calleeMethodName, arguments,
                            methodCallPossibleInformation);
                }
            }
        }

        // 处理一般的被调用对象类型
        List<String> possibleTypeList = methodCallPossibleInfoEntry4Object.getPossibleTypeList();
        if (possibleTypeList != null) {
            for (String possibleType : possibleTypeList) {
                // 尝试添加方法调用信息，使用对应的被调用类型
                tryAddMethodCallWithType(isInterface, false, handledCalleeTypeSet, calleeClassName, Collections.singletonList(possibleType), calleeMethodName, arguments,
                        methodCallPossibleInformation);
            }
        }

        if (handledCalleeTypeSet.isEmpty()) {
            // 未添加与方法调用指令中被调用类不同类型的调用信息，使用方法调用指令中被调用类进行添加
            // 获取实际的被调用类型
            JavaCGCallTypeEnum callTypeEnum = isInterface ? JavaCGCallTypeEnum.CTE_RAW_INVOKE_INTERFACE : JavaCGCallTypeEnum.CTE_RAW_INVOKE_VIRTUAL;

            // 记录方法调用信息
            addCommonMethodCallWithInfo(callTypeEnum, null, calleeClassName, calleeMethodName, arguments, methodCallPossibleInformation);
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
     * @param isInterface
     * @param isSpringBean
     * @param handledCalleeTypeSet
     * @param calleeClassName
     * @param calleeTypeRuntimeList
     * @param calleeMethodName
     * @param arguments
     * @param methodCallPossibleInformation
     */
    private void tryAddMethodCallWithType(boolean isInterface,
                                          boolean isSpringBean,
                                          Set<String> handledCalleeTypeSet,
                                          String calleeClassName,
                                          List<String> calleeTypeRuntimeList,
                                          String calleeMethodName,
                                          Type[] arguments,
                                          MethodCallPossibleInformation methodCallPossibleInformation) {
        for (String calleeTypeRuntime : calleeTypeRuntimeList) {
            if (handledCalleeTypeSet.contains(calleeTypeRuntime) ||
                    StringUtils.equals(calleeClassName, calleeTypeRuntime) ||
                    JavaCGByteCodeUtil.isNullType(calleeTypeRuntime) ||
                    JavaCGCommonNameConstants.CLASS_NAME_OBJECT.equals(calleeTypeRuntime)) {
                /*
                    以下情况不处理：
                        已处理过的被调用类型
                        被调用类型与方法调用指令中被调用类相同
                        被调用类型为null
                        被调用类型为Object
                 */
                continue;
            }

            // 获取实际的被调用类型
            JavaCGCallTypeEnum callTypeEnum = chooseActualCallType(isInterface, isSpringBean);

            // 记录方法调用信息
            addCommonMethodCallWithInfo(callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, arguments, methodCallPossibleInformation);
            handledCalleeTypeSet.add(calleeTypeRuntime);
        }
    }

    /**
     * 记录方法调用信息，处理可能的信息
     *
     * @param callTypeEnum                  调用类型
     * @param calleeTypeRuntime             运行时的被调用类型
     * @param calleeClassName               方法调用指令中的被调用类名
     * @param calleeMethodName              被调用方法名
     * @param arguments                     被调用方法参数
     * @param methodCallPossibleInformation 方法调用可能的信息
     */
    private void addCommonMethodCallWithInfo(JavaCGCallTypeEnum callTypeEnum,
                                             String calleeTypeRuntime,
                                             String calleeClassName,
                                             String calleeMethodName,
                                             Type[] arguments,
                                             MethodCallPossibleInformation methodCallPossibleInformation) {
        MethodCall methodCall = addCommonMethodCall(callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, arguments, methodCallPossibleInformation.getObjTypeEnum());
        if (methodCall == null) {
            return;
        }

        methodCallInfoMap.put(methodCall.getCallId(), methodCallPossibleInformation);
    }

    /**
     * 记录方法调用信息
     *
     * @param callTypeEnum      调用类型
     * @param calleeTypeRuntime 运行时的被调用类型
     * @param calleeClassName   方法调用指令中的被调用类名
     * @param calleeMethodName  被调用方法名
     * @param arguments         被调用方法参数
     * @param objTypeEnum       被调用对象类型
     */
    private MethodCall addCommonMethodCall(JavaCGCallTypeEnum callTypeEnum,
                                           String calleeTypeRuntime,
                                           String calleeClassName,
                                           String calleeMethodName,
                                           Type[] arguments,
                                           JavaCGCalleeObjTypeEnum objTypeEnum) {
        if (JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return null;
        }

        // 若运行时的被调用类型非空则使用，若为空则使用方法调用指令中的被调用类名
        MethodCall methodCall = new MethodCall(callerClassName, callerMethodName, callerMethodArgs, callTypeEnum, (calleeTypeRuntime != null ? calleeTypeRuntime :
                calleeClassName), calleeMethodName, JavaCGByteCodeUtil.getArgListStr(arguments), getSourceLine(), objTypeEnum);
        methodCallList.addMethodCall(methodCall);

        return methodCall;
    }

    // 添加其他方法调用关系
    private void addOtherMethodCall(String callerClassName,
                                    String callerMethodName,
                                    String callerMethodArgs,
                                    JavaCGCallTypeEnum methodCallType,
                                    String calleeClassName,
                                    String calleeMethodName,
                                    String calleeMethodArgs,
                                    int callerSourceLine) {
        if (JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return;
        }

        MethodCall methodCall = new MethodCall(callerClassName, callerMethodName, callerMethodArgs, methodCallType, calleeClassName, calleeMethodName, calleeMethodArgs,
                callerSourceLine, null);
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
        if (!JavaCGConstants.METHOD_NAME_INIT.equals(calleeMethodName)) {
            // 记录原始的方法调用类型
            return false;
        }

        boolean skipRawMethodCall = false;
        String calleeMethodArgs = JavaCGByteCodeUtil.getArgListStr(arguments);

        // 处理Runnable实现类
        if (handleSpecialInitMethod(runnableImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_RUNNABLE_INIT_RUN1,
                JavaCGCallTypeEnum.CTE_RUNNABLE_INIT_RUN2, JavaCGCommonNameConstants.METHOD_RUNNABLE_RUN, JavaCGConstants.EMPTY_METHOD_ARGS)) {
            skipRawMethodCall = true;
        }

        // 处理Callable实现类
        if (handleSpecialInitMethod(callableImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_CALLABLE_INIT_CALL1,
                JavaCGCallTypeEnum.CTE_CALLABLE_INIT_CALL2, JavaCGCommonNameConstants.METHOD_CALLABLE_CALL, JavaCGConstants.EMPTY_METHOD_ARGS)) {
            skipRawMethodCall = true;
        }

        // 处理TransactionCallback实现类
        if (handleSpecialInitMethod(transactionCallbackImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_TX_CALLBACK_INIT_CALL1,
                JavaCGCallTypeEnum.CTE_TX_CALLBACK_INIT_CALL2, JavaCGCommonNameConstants.METHOD_DO_IN_TRANSACTION, JavaCGCommonNameConstants.ARGS_TRANSACTION_STATUS)) {
            skipRawMethodCall = true;
        }

        // 处理TransactionCallbackWithoutResult实现类
        if (handleSpecialInitMethod(transactionCallbackWithoutResultChildClassMap, calleeClassName, calleeMethodName, calleeMethodArgs,
                JavaCGCallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL1, JavaCGCallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL2,
                JavaCGCommonNameConstants.METHOD_DO_IN_TRANSACTION_WITHOUT_RESULT,
                JavaCGCommonNameConstants.ARGS_TRANSACTION_STATUS)) {
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
                                            String addedCalleeMethodArgs) {
        Boolean recorded = map.get(calleeClassName);
        if (recorded == null) {
            return false;
        }

        // 记录其他方法调用对应类的<init>方法
        addOtherMethodCall(callerClassName, callerMethodName, callerMethodArgs, callTypeEnum1, calleeClassName, calleeMethodName, calleeMethodArgs, getSourceLine());

        if (Boolean.FALSE.equals(recorded)) {
            // 对应类的<init>方法调用需要增加的方法
            addOtherMethodCall(calleeClassName, calleeMethodName, calleeMethodArgs, callTypeEnum2, calleeClassName, addedCalleeMethodName, addedCalleeMethodArgs,
                    JavaCGConstants.DEFAULT_LINE_NUMBER);
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
        if (!JavaCGConstants.METHOD_NAME_START.equals(calleeMethodName) || arguments.length > 0) {
            // 被调用方法不是start()，返回
            return;
        }

        // 处理Thread子类
        if (!Boolean.FALSE.equals(threadChildClassMap.get(calleeClassName))) {
            return;
        }

        String calleeMethodArgs = JavaCGByteCodeUtil.getArgListStr(arguments);
        // 记录Thread子类的start方法调用run方法（以上Map的value等于FALSE时，代表当前类为Thread的子类，且start()方法调用run()方法未添加过）
        addOtherMethodCall(calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGCallTypeEnum.CTE_THREAD_START_RUN,
                calleeClassName, "run", JavaCGConstants.EMPTY_METHOD_ARGS, JavaCGConstants.DEFAULT_LINE_NUMBER);
        // 避免start()方法调用run()方法被添加多次
        threadChildClassMap.put(calleeClassName, Boolean.TRUE);
    }

    // 处理方法调用可能的信息
    private void handleMethodCallPossibleInformation(int methodCallId, MethodCallPossibleInformation methodCallPossibleInformation) throws IOException {
        if (methodCallPossibleInformation == null) {
            return;
        }

        // 记录方法调用可能的信息，被调用对象
        recordMethodCallPossibleInfo(methodCallId, JavaCGConstants.METHOD_CALL_POSSIBLE_INFO_SEQ_OBJECT, methodCallPossibleInformation.getPossibleInfo4Object());

        // 记录方法调用可能的信息，参数
        for (int i = 0; i < methodCallPossibleInformation.getPossibleInfoNum4Args(); i++) {
            recordMethodCallPossibleInfo(methodCallId, JavaCGConstants.METHOD_CALL_POSSIBLE_INFO_SEQ_ARGS_START + i, methodCallPossibleInformation.getPossibleInfo4Args(i));
        }
    }

    // 记录方法调用可能的信息
    private void recordMethodCallPossibleInfo(int methodCallId, int argSeq, MethodCallPossibleInfoEntry methodCallPossibleInfoEntry) throws IOException {
        if (methodCallPossibleInfoEntry == null) {
            return;
        }

        StringBuilder stringBuilder = new StringBuilder();
        /*
            文件格式：
            方法调用ID	参数序号	类型（类型/值/base64后的值/静态变量）	值的序号	值（类型/值/base64后的值/静态变量）
         */

        // 处理类型
        recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleInfoEntry.getPossibleTypeList(), methodCallId, argSeq,
                JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_TYPE);

        // 处理值
        List<Object> possibleValueList = methodCallPossibleInfoEntry.getPossibleValueList();
        if (possibleValueList != null) {
            for (int i = 0; i < possibleValueList.size(); i++) {
                stringBuilder.append(methodCallId).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                        append(argSeq).append(JavaCGConstants.FILE_COLUMN_SEPARATOR);
                String base64Value = null;
                Object value = possibleValueList.get(i);
                if (value instanceof String) {
                    String strValue = (String) value;
                    // 假如值中包含可能导致文件解析时格式不符合预期的字符，则需要进行base64编码
                    if (StringUtils.containsAny(strValue, "\r", "\n", "\t")) {
                        base64Value = JavaCGUtil.base64Encode(strValue);
                    }
                }
                if (base64Value == null) {
                    stringBuilder.append(JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_VALUE);
                } else {
                    stringBuilder.append(JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_BASE64_VALUE);
                }

                stringBuilder.append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                        append(i).append(JavaCGConstants.FILE_COLUMN_SEPARATOR);

                if (base64Value == null) {
                    stringBuilder.append(value);
                } else {
                    stringBuilder.append(base64Value);
                }
                stringBuilder.append(JavaCGConstants.NEW_LINE);
            }
        }

        // 处理静态变量
        recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleInfoEntry.getPossibleStaticFieldClassAndFieldNameList(), methodCallId, argSeq,
                JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD);

        // 处理被调用对象或参数是静态字段方法返回值的可能信息
        recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleInfoEntry.getPossibleStaticFieldMethodCallList(), methodCallId, argSeq,
                JavaCGConstants.FILE_KEY_METHOD_CALL_POSSIBLE_INFO_STATIC_FIELD_METHOD_CALL);

        JavaCGFileUtil.write2FileNoLF(methodCallInfoWriter, stringBuilder.toString());
    }

    private void recordStringMethodCallPossibleInfo(StringBuilder stringBuilder, List<String> stringList, int methodCallId, int argSeq, String type) {
        if (stringList == null) {
            return;
        }

        for (int i = 0; i < stringList.size(); i++) {
            stringBuilder.append(methodCallId).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                    append(argSeq).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                    append(type).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                    append(i).append(JavaCGConstants.FILE_COLUMN_SEPARATOR).
                    append(stringList.get(i)).append(JavaCGConstants.NEW_LINE);
        }
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
}
