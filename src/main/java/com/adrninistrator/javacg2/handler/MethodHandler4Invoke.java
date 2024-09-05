package com.adrninistrator.javacg2.handler;

import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.TypeConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeObjTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfInfo;
import com.adrninistrator.javacg2.dto.call.MethodCall;
import com.adrninistrator.javacg2.dto.call.MethodCallList;
import com.adrninistrator.javacg2.dto.call.MethodCallPossibleEntry;
import com.adrninistrator.javacg2.dto.call.MethodCallPossibleInfo;
import com.adrninistrator.javacg2.dto.call.MethodCallPossibleList;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.exception.CatchAndFinallyInfo;
import com.adrninistrator.javacg2.dto.exception.CatchInfo;
import com.adrninistrator.javacg2.dto.exception.FinallyInfo;
import com.adrninistrator.javacg2.dto.exception.ThrowInfo;
import com.adrninistrator.javacg2.dto.exception.ThrowInfoList;
import com.adrninistrator.javacg2.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg2.dto.field.StaticFieldTypeAndName;
import com.adrninistrator.javacg2.dto.fieldrelationship.GetSetFieldRelationship;
import com.adrninistrator.javacg2.dto.instruction.InvokeInstructionPosAndCallee;
import com.adrninistrator.javacg2.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg2.dto.method.JavaCG2MethodInfo;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.extensions.annotationattributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg2.extensions.codeparser.MethodAnnotationParser;
import com.adrninistrator.javacg2.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg2.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg2.util.JavaCG2AnnotationUtil;
import com.adrninistrator.javacg2.util.JavaCG2BootstrapMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2InstructionUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.BootstrapMethod;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantInvokeDynamic;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumber;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.ParameterAnnotationEntry;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.CHECKCAST;
import org.apache.bcel.generic.CodeExceptionGen;
import org.apache.bcel.generic.GETSTATIC;
import org.apache.bcel.generic.GotoInstruction;
import org.apache.bcel.generic.INVOKEDYNAMIC;
import org.apache.bcel.generic.INVOKEINTERFACE;
import org.apache.bcel.generic.INVOKESPECIAL;
import org.apache.bcel.generic.INVOKESTATIC;
import org.apache.bcel.generic.INVOKEVIRTUAL;
import org.apache.bcel.generic.IfInstruction;
import org.apache.bcel.generic.Instruction;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.InvokeInstruction;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
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

    private static final Logger logger = LoggerFactory.getLogger(MethodHandler4Invoke.class);

    private final String callerMethodArgTypes;

    private final UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    private final JavaCG2Counter callIdCounter;

    protected final InstructionList instructionList;

    // 保存方法之间调用关系
    private final MethodCallList methodCallList;

    // 当前类是否为内部类
    private final boolean innerAnonymousClassFlag;

    /*
        保存方法调用可能的信息
        key
            方法调用ID
        value
            方法调用可能的信息
     */
    private final Map<Integer, MethodCallPossibleInfo> methodCallInfoMap = new HashMap<>(10);

    private MethodHandler4TypeAndValue methodHandler4TypeAndValue;

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
    private Writer methodArgAnnotationWriter;
    private Writer methodLineNumberWriter;
    private Writer methodCallInfoWriter;
    private Writer methodCallMethodCallReturnWriter;
    private Writer methodCallStaticFieldWriter;
    private Writer methodReturnArgSeqWriter;
    private Writer methodReturnCallIdWriter;
    private Writer methodCatchWriter;
    private Writer methodFinallyWriter;
    private Writer methodThrowWriter;
    private Writer staticFinalFieldMethodCallIdWriter;
    private Writer fieldRelationshipWriter;

    private int lastJarNum;

    // 是否出现方法名+参数类型均相同的方法标记
    private boolean existsSameMethodNameAndArgs;

    private ClassAndJarNum classAndJarNum;

    /*
        方法调用指令位置与call_id对应关系Map
        key     方法调用指令的position
        value   方法调用指令的call_id
     */
    private Map<Integer, Integer> invokeInstructionPositionCallIdMap;

    /*
        记录当前类的static、final字段名称及对应的类型的Map
        key     当前类的static、final字段名称
        value   当前类的static、final字段类型
     */
    private Map<String, String> staticFinalFieldNameTypeMap;

    public MethodHandler4Invoke(Method method,
                                MethodGen mg,
                                JavaClass javaClass,
                                JavaCG2ConfInfo javaCG2ConfInfo,
                                String callerMethodArgTypes,
                                String callerFullMethod,
                                UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler,
                                JavaCG2Counter callIdCounter) {
        super(method, mg, javaClass, callerFullMethod, javaCG2ConfInfo);

        this.callerMethodArgTypes = callerMethodArgTypes;
        this.useSpringBeanByAnnotationHandler = useSpringBeanByAnnotationHandler;
        this.callIdCounter = callIdCounter;

        instructionList = mg.getInstructionList();
        methodCallList = new MethodCallList(callIdCounter);

        innerAnonymousClassFlag = JavaCG2ByteCodeUtil.checkInnerAnonymousClass(javaClass);
    }

    // 初始化
    @Override
    protected void init() {
        if (parseMethodCallTypeValueFlag) {
            methodReturnArgSeqList = new ArrayList<>(1);
            methodReturnPositionList = new ArrayList<>(1);
            methodReturnArgSeqEQCList = new ArrayList<>(1);
            methodReturnPositionEQCList = new ArrayList<>(1);
            getSetFieldRelationshipList = new ArrayList<>();
        }
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
            JavaCG2AnnotationUtil.writeAnnotationInfo(methodAnnotationWriter,
                    method.getAnnotationEntries(),
                    annotationAttributesFormatter,
                    callerFullMethod);

            // 记录方法参数上的注解信息
            ParameterAnnotationEntry[] parameterAnnotationEntries = method.getParameterAnnotationEntries();
            if (ArrayUtils.isNotEmpty(parameterAnnotationEntries)) {
                for (int i = 0; i < parameterAnnotationEntries.length; i++) {
                    ParameterAnnotationEntry parameterAnnotationEntry = parameterAnnotationEntries[i];
                    if (parameterAnnotationEntry == null) {
                        continue;
                    }
                    JavaCG2AnnotationUtil.writeAnnotationInfo(methodArgAnnotationWriter,
                            parameterAnnotationEntry.getAnnotationEntries(),
                            annotationAttributesFormatter,
                            callerFullMethod,
                            String.valueOf(i));
                }
            }

            // 处理方法的行号信息
            handleLineNumber(callerFullMethod);
        }

        // 处理方法注解，需要在此执行，否则接口方法上的注解不会被处理
        handleMethodAnnotations();

        // 初始化当前处理的指令
        ih = JavaCG2InstructionUtil.getFirstInstructionHandle(mg);
        // 若方法中指令为空，不需要再判断方法是否为abstract或native
        return ih != null;
    }

    @Override
    protected boolean doHandleMethod() throws IOException {
        if (parseMethodCallTypeValueFlag) {
            // 需要获取方法调用指令对应的类型与值
            methodHandler4TypeAndValue = new MethodHandler4TypeAndValue(method, mg, javaClass, callerFullMethod, javaCG2ConfInfo);
            methodHandler4TypeAndValue.setFailCounter(failCounter);
            methodHandler4TypeAndValue.setParseMethodCallTypeValueFlag(true);
            methodHandler4TypeAndValue.setGetMethodWriter(getMethodWriter);
            methodHandler4TypeAndValue.setSetMethodWriter(setMethodWriter);
            methodHandler4TypeAndValue.setFieldGenericsTypeWriter(fieldGenericsTypeWriter);
            methodHandler4TypeAndValue.setRecordedSetMethodSet(recordedSetMethodSet);
            methodHandler4TypeAndValue.setNonStaticFieldNameTypeMap(nonStaticFieldNameTypeMap);
            methodHandler4TypeAndValue.setNonStaticFieldNameGenericsTypeMap(nonStaticFieldNameGenericsTypeMap);
            methodHandler4TypeAndValue.setRecordedFieldWithGenericsTypeSet(recordedFieldWithGenericsTypeSet);
            methodHandler4TypeAndValue.setMethodReturnArgSeqList(methodReturnArgSeqList);
            methodHandler4TypeAndValue.setMethodReturnPositionList(methodReturnPositionList);
            methodHandler4TypeAndValue.setMethodReturnArgSeqEQCList(methodReturnArgSeqEQCList);
            methodHandler4TypeAndValue.setMethodReturnPositionEQCList(methodReturnPositionEQCList);
            methodHandler4TypeAndValue.setOnlyAnalyseReturnTypeFlag(false);
            if (inClinitMethod) {
                methodHandler4TypeAndValue.setInClinitMethod(true);
                methodHandler4TypeAndValue.setSfFieldInvokeInstructionMap(sfFieldInvokeInstructionMap);
            }
            if (javaCG2ConfInfo.isFirstParseInitMethodType()) {
                // 处理类的方法前需要先解析构造函数以非静态字段可能的类型
                methodHandler4TypeAndValue.setRecordFieldPossibleTypeFlag(false);
                methodHandler4TypeAndValue.setUseFieldPossibleTypeFlag(true);
                methodHandler4TypeAndValue.setNonStaticFieldPossibleTypes(nonStaticFieldPossibleTypes);
            }
            if (javaCG2ConfInfo.isAnalyseFieldRelationship()) {
                // 需要分析dto的字段之间的关联关系
                methodHandler4TypeAndValue.setAnalyseFieldRelationshipFlag(true);
                methodHandler4TypeAndValue.setFieldRelationshipCounter(fieldRelationshipCounter);
                methodHandler4TypeAndValue.setGetSetFieldRelationshipList(getSetFieldRelationshipList);
            }
            // 获取方法调用指令对应的类型与值
            if (!methodHandler4TypeAndValue.handleMethod()) {
                return false;
            }
            // 在遍历指令之前创建
            invokeInstructionPositionCallIdMap = new HashMap<>();
        }

        // 遍历指令
        do {
            short opCode = ih.getInstruction().getOpcode();
            if (JavaCG2InstructionUtil.isMethodInvokeInstruction(opCode)) {
                // 处理方法调用指令
                MethodCallPossibleInfo methodCallPossibleInfo = null;
                if (methodHandler4TypeAndValue != null) {
                    // 获取方法调用指令对应的类型与值
                    methodCallPossibleInfo = methodHandler4TypeAndValue.getMethodCallPossibleInfo(ih.getPosition());
                }
                handleInvokeInstruction(methodCallPossibleInfo);
            } else if (Const.ATHROW == opCode) {
                if (methodHandler4TypeAndValue != null) {
                    ThrowInfoList throwInfoList = methodHandler4TypeAndValue.getMethodThrowPossibleInfo(ih.getPosition());
                    if (throwInfoList != null) {
                        // 处理ATHROW指令
                        handleATHROW(throwInfoList);
                    }
                }
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
            JavaCG2FileUtil.write2FileWithTab(methodCallWriter, methodCall.genMethodCallContent(String.valueOf(lastJarNum), calleeClassJarNum));
            if (parseMethodCallTypeValueFlag) {
                // 处理方法调用可能的信息
                handleMethodCallPossibleInfo(methodCall, methodCallInfoMap.get(methodCall.getCallId()));
            }
        }

        if (parseMethodCallTypeValueFlag) {
            // 记录方法返回值对应的方法参数序号
            for (Integer methodReturnArgSeq : methodReturnArgSeqList) {
                JavaCG2FileUtil.write2FileWithTab(methodReturnArgSeqWriter, callerFullMethod, String.valueOf(methodReturnArgSeq), JavaCG2YesNoEnum.NO.getStrValue());
            }

            // 记录方法返回值对应的方法调用ID
            for (Integer methodReturnPosition : methodReturnPositionList) {
                Integer methodReturnCallId = getInvokeInstructionCallId(methodReturnPosition);
                JavaCG2FileUtil.write2FileWithTab(methodReturnCallIdWriter, callerFullMethod, String.valueOf(methodReturnCallId), JavaCG2YesNoEnum.NO.getStrValue());
            }

            // 记录等值转换前方法返回值对应的方法参数序号
            for (Integer methodReturnArgSeqEQC : methodReturnArgSeqEQCList) {
                if (!methodReturnArgSeqList.contains(methodReturnArgSeqEQC)) {
                    // 等值转换前的方法参数序号，若在非等值转换的情况下也存在则不写入，避免重复
                    JavaCG2FileUtil.write2FileWithTab(methodReturnArgSeqWriter, callerFullMethod, String.valueOf(methodReturnArgSeqEQC), JavaCG2YesNoEnum.YES.getStrValue());
                }
            }

            // 记录等值转换前方法返回值对应的方法调用ID
            for (Integer methodReturnPositionEQC : methodReturnPositionEQCList) {
                if (!methodReturnPositionList.contains(methodReturnPositionEQC)) {
                    // 等值转换前的方法调用ID，若在非等值转换的情况下也存在则不写入，避免重复
                    Integer methodReturnCallIdEQC = getInvokeInstructionCallId(methodReturnPositionEQC);
                    JavaCG2FileUtil.write2FileWithTab(methodReturnCallIdWriter, callerFullMethod, String.valueOf(methodReturnCallIdEQC), JavaCG2YesNoEnum.YES.getStrValue());
                }
            }

            if (inClinitMethod && !sfFieldInvokeInstructionMap.isEmpty()) {
                // 记录当前类的static、final字段名称及初始化方法call_id
                List<String> staticFinalFieldNameList = new ArrayList<>(sfFieldInvokeInstructionMap.keySet());
                Collections.sort(staticFinalFieldNameList);
                for (String staticFinalFieldName : staticFinalFieldNameList) {
                    String staticFinalFieldType = staticFinalFieldNameTypeMap.get(staticFinalFieldName);
                    if (JavaCG2Util.checkSkipClass(staticFinalFieldType, javaCG2ConfInfo.getNeedHandlePackageSet())) {
                        // 假如static、final字段类型不需要处理，则不记录到文件
                        continue;
                    }

                    List<InvokeInstructionPosAndCallee> invokeInstructionPosAndCalleeList = sfFieldInvokeInstructionMap.get(staticFinalFieldName);
                    for (int i = 0; i < invokeInstructionPosAndCalleeList.size(); i++) {
                        InvokeInstructionPosAndCallee invokeInstructionPosAndCallee = invokeInstructionPosAndCalleeList.get(i);
                        if (JavaCG2Util.checkSkipClass(invokeInstructionPosAndCallee.getCalleeClassName(), javaCG2ConfInfo.getNeedHandlePackageSet())) {
                            // 假如static、final字段初始化方法被调用类名不需要处理，则不记录到文件
                            continue;
                        }

                        // 根据方法调用指令位置查找对应的call_id
                        Integer sffMethodCallId = getInvokeInstructionCallId(invokeInstructionPosAndCallee.getInvokeInstructionPosition());
                        JavaCG2FileUtil.write2FileWithTab(staticFinalFieldMethodCallIdWriter, callerClassName, staticFinalFieldName, String.valueOf(i),
                                String.valueOf(sffMethodCallId), staticFinalFieldType, invokeInstructionPosAndCallee.getCalleeClassName(),
                                invokeInstructionPosAndCallee.getCalleeMethodName());
                    }
                }
            }

            // 记录通过get/set方法关联的字段关系到文件
            for (GetSetFieldRelationship getSetFieldRelationship : getSetFieldRelationshipList) {
                int getMethodCallId = getInvokeInstructionCallId(getSetFieldRelationship.getGetInvokeInstructionPosition());
                int setMethodCallId = getInvokeInstructionCallId(getSetFieldRelationship.getSetInvokeInstructionPosition());
                JavaCG2FileUtil.write2FileWithTab(fieldRelationshipWriter,
                        String.valueOf(getSetFieldRelationship.getRecordId()),
                        String.valueOf(getMethodCallId),
                        String.valueOf(setMethodCallId),
                        callerFullMethod,
                        String.valueOf(getSetFieldRelationship.getCallerLineNumber()),
                        getSetFieldRelationship.getGetClassName(),
                        getSetFieldRelationship.getGetMethodName(),
                        getSetFieldRelationship.getSetClassName(),
                        getSetFieldRelationship.getSetMethodName(),
                        getSetFieldRelationship.getValid(),
                        getSetFieldRelationship.getType().getType());
            }
        }

        // 记录异常处理信息
        recordExceptionInfo();
        return true;
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
            // https://github.com/Adrninistrator/java-callgraph2/pull/44/commits/75f41e71dbe43103b673ab296f0d42bda537cd42
            // 寻找行号中的最大值
            for (LineNumber lineNumber : lineNumbers) {
                int sourceLineNumber = lineNumber.getLineNumber();
                if (maxLineNumber < sourceLineNumber) {
                    maxLineNumber = sourceLineNumber;
                }
            }
        }

        // 记录方法起始代码行号
        JavaCG2FileUtil.write2FileWithTab(methodLineNumberWriter, callerFullMethod, String.valueOf(minLineNumber), String.valueOf(maxLineNumber));
    }

    // 记录异常处理信息
    private void recordExceptionInfo() throws IOException {
        if (!parseMethodCallTypeValueFlag) {
            // 以下处理需要使用方法调用中被调用对象与参数的类型与值，假如开关未打开则不记录异常处理相关信息
            return;
        }
        CodeExceptionGen[] codeExceptionGens = mg.getExceptionHandlers();
        if (codeExceptionGens.length <= 0) {
            return;
        }

        // 记录catch信息的列表
        List<CatchInfo> catchInfoList = new ArrayList<>();
        // 记录catch开始偏移量的Set
        Set<Integer> catchFromPositionSet = new HashSet<>();
        /*
            记录try、catch、finally信息的Map
            key     try的开始指令偏移
            value   try对应的catch、finally信息
         */
        Map<Integer, CatchAndFinallyInfo> tryInfoMap = new HashMap<>();
        // 记录finally信息的列表
        List<FinallyInfo> finallyInfoList = new ArrayList<>();
        /*
            记录catch、finally信息的Map
            key     catch的开始指令偏移
            value   catch对应的finally信息
         */
        Map<Integer, FinallyInfo> catchFinallyInfoMap = new HashMap<>();

        // 记录上一条处理的catch信息
        CatchInfo lastCatchInfo = null;
        for (CodeExceptionGen codeExceptionGen : codeExceptionGens) {
            int fromPosition = codeExceptionGen.getStartPC().getPosition();
            // BCEL的CodeExceptionGen的end的下一个指令，对应Exception table的to
            int toPosition = codeExceptionGen.getEndPC().getNext().getPosition();
            // BCEL的CodeExceptionGen的end，对应Exception table的to的上一个指令
            int endPosition = codeExceptionGen.getEndPC().getPosition();
            int targetPosition = codeExceptionGen.getHandlerPC().getPosition();
            if (fromPosition == targetPosition) {
                // 假如from与target的偏移量相同则不处理
                continue;
            }

            if (endPosition == targetPosition) {
                // 假如end与target的偏移量相同，则end使用end前一个指令的偏移
                endPosition = codeExceptionGen.getEndPC().getPrev().getPosition();
            }
            ObjectType catchExceptionType = codeExceptionGen.getCatchType();
            boolean isFinally = (catchExceptionType == null);
            CatchAndFinallyInfo catchAndFinallyInfo = tryInfoMap.computeIfAbsent(fromPosition, k -> new CatchAndFinallyInfo());
            if (!isFinally) {
                // catch信息
                String catchExceptionTypeString = catchExceptionType.getClassName();
                if (lastCatchInfo != null && lastCatchInfo.getTargetPosition() == targetPosition) {
                    // 当前的catch信息与上一条catch的开始指令偏移量相同，则不创建新的catch信息，向上一条catch信息的异常类型中添加记录
                    lastCatchInfo.addCatchExceptionType(catchExceptionTypeString);
                } else {
                    CatchInfo catchInfo = new CatchInfo(fromPosition, toPosition, endPosition, targetPosition, catchExceptionTypeString);
                    catchInfoList.add(catchInfo);
                    catchFromPositionSet.add(targetPosition);
                    catchAndFinallyInfo.getCatchInfoList().add(catchInfo);
                    lastCatchInfo = catchInfo;
                }
            } else {
                // finally信息
                FinallyInfo finallyInfo = new FinallyInfo(fromPosition, toPosition, endPosition, targetPosition);
                finallyInfoList.add(finallyInfo);
                catchAndFinallyInfo.setFinallyInfo(finallyInfo);
                if (catchFromPositionSet.contains(fromPosition)) {
                    catchFinallyInfoMap.put(fromPosition, finallyInfo);
                }
                // 处理finally信息，把上一条catch信息清空
                lastCatchInfo = null;
            }
        }
        // 将catch信息写入文件
        for (CatchInfo catchInfo : catchInfoList) {
            // 获取catch代码块的结束偏移量
            int catchEndPosition = getCatchEndPosition(catchInfo, catchFinallyInfoMap, tryInfoMap);
            int tryStartLineNumber = getSourceLine(catchInfo.getFromPosition());
            int tryEndLineNumber = getSourceLine(catchInfo.getEndPosition());
            int tryMinCallId = getMinCallId(catchInfo.getFromPosition(), catchInfo.getEndPosition());
            int tryMaxCallId = getMaxCallId(catchInfo.getFromPosition(), catchInfo.getEndPosition());
            int catchStartLineNumber = getSourceLine(catchInfo.getTargetPosition());
            int catchEndLineNumber = getSourceLine(catchEndPosition);
            int catchMinCallId = getMinCallId(catchInfo.getTargetPosition(), catchEndPosition);
            int catchMaxCallId = getMaxCallId(catchInfo.getTargetPosition(), catchEndPosition);
            // 获取catch代码块的标识
            String flag = getCatchFlag(catchInfo, catchEndPosition, tryMinCallId, tryMaxCallId, catchMinCallId, catchMaxCallId);
            for (String catchExceptionType : catchInfo.getCatchExceptionTypeList()) {
                JavaCG2FileUtil.write2FileWithTab(methodCatchWriter, callerFullMethod, catchExceptionType, flag, String.valueOf(tryStartLineNumber),
                        String.valueOf(tryEndLineNumber), String.valueOf(tryMinCallId), String.valueOf(tryMaxCallId), String.valueOf(catchInfo.getTargetPosition()),
                        String.valueOf(catchEndPosition), String.valueOf(catchStartLineNumber), String.valueOf(catchEndLineNumber), String.valueOf(catchMinCallId),
                        String.valueOf(catchMaxCallId));
            }
        }
        // 将finally信息写入文件
        for (FinallyInfo finallyInfo : finallyInfoList) {
            boolean isTryOrCatch = !catchFromPositionSet.contains(finallyInfo.getFromPosition());
            String tryCatchType = isTryOrCatch ? JavaCG2Constants.TRY : JavaCG2Constants.CATCH;
            int tryCatchStartLineNumber = getSourceLine(finallyInfo.getFromPosition());
            int tryCatchEndLineNumber = getSourceLine(finallyInfo.getEndPosition());
            int tryCatchMinCallId = getMinCallId(finallyInfo.getFromPosition(), finallyInfo.getEndPosition());
            int tryCatchMaxCallId = getMaxCallId(finallyInfo.getFromPosition(), finallyInfo.getEndPosition());
            int finallyStartLineNumber = getSourceLine(finallyInfo.getTargetPosition());
            JavaCG2FileUtil.write2FileWithTab(methodFinallyWriter, callerFullMethod, tryCatchType, String.valueOf(tryCatchStartLineNumber), String.valueOf(tryCatchEndLineNumber),
                    String.valueOf(tryCatchMinCallId), String.valueOf(tryCatchMaxCallId), String.valueOf(finallyStartLineNumber));
        }
    }

    // 获取catch代码块的标识
    private String getCatchFlag(CatchInfo catchInfo, int catchEndPosition, int tryMinCallId, int tryMaxCallId, int catchMinCallId, int catchMaxCallId) {
        if (innerAnonymousClassFlag && JavaCG2CommonNameConstants.METHOD_NAME_CLINIT.equals(callerMethodName)) {
            // 当前类为内部类，且方法为<clinit>时，判断catch代码块是否为编译器生成的switch处理
            InstructionHandle tryStartIh = instructionList.findHandle(catchInfo.getFromPosition());
            Instruction tryStartInstruction = tryStartIh.getInstruction();
            if (tryStartInstruction.getOpcode() == Const.GETSTATIC) {
                GETSTATIC getstatic = (GETSTATIC) tryStartInstruction;
                String getStaticFieldName = getstatic.getFieldName(cpg);
                if (getStaticFieldName.startsWith(JavaCG2CommonNameConstants.SWITCH_MAP)) {
                    return JavaCG2Constants.FILE_KEY_CATCH_FLAG_SWITCH;
                }
            }
        }

        // 判断是否编译器为try-with-resource生成的catch代码块
        if (tryMinCallId == tryMaxCallId && tryMinCallId >= JavaCG2Constants.METHOD_CALL_ID_MIN && catchMinCallId == catchMaxCallId && catchMinCallId >= JavaCG2Constants.METHOD_CALL_ID_MIN) {
            // try与catch代码块应都只有一个方法调用，且指令为INVOKEVIRTUAL
            InstructionHandle tryInvokeVirtualIh = JavaCG2InstructionUtil.findFirstIhBetween(instructionList, catchInfo.getFromPosition(), catchInfo.getToPosition(),
                    Const.INVOKEVIRTUAL);
            if (tryInvokeVirtualIh == null) {
                return "";
            }
            INVOKEVIRTUAL tryInvokeVirtual = (INVOKEVIRTUAL) tryInvokeVirtualIh.getInstruction();
            String tryCalleeMethodName = tryInvokeVirtual.getMethodName(cpg);
            // try中的方法调用被调用方法名应为close
            if (!JavaCG2CommonNameConstants.METHOD_NAME_CLOSE.equals(tryCalleeMethodName)) {
                return "";
            }

            InstructionHandle catchInvokeVirtualIh = JavaCG2InstructionUtil.findFirstIhBetween(instructionList, catchInfo.getTargetPosition(), catchEndPosition,
                    Const.INVOKEVIRTUAL);
            if (catchInvokeVirtualIh == null) {
                return "";
            }
            INVOKEVIRTUAL catchInvokeVirtual = (INVOKEVIRTUAL) catchInvokeVirtualIh.getInstruction();
            JavaCG2MethodInfo catchCalleeInfo = JavaCG2InstructionUtil.getCalleeMethodInfo(catchInvokeVirtual, cpg);
            // catch中的方法调用被调用方法应为 Throwable.addSuppressed(Throwable)
            if (!JavaCG2CommonNameConstants.CLASS_NAME_THROWABLE.equals(catchCalleeInfo.getClassName()) ||
                    !JavaCG2CommonNameConstants.METHOD_NAME_ADD_SUPPRESSED.equals(catchCalleeInfo.getMethodName()) ||
                    catchCalleeInfo.getMethodArgumentTypes().length != 1 ||
                    !JavaCG2CommonNameConstants.CLASS_NAME_THROWABLE.equals(catchCalleeInfo.getMethodArgumentTypes()[0].toString())) {
                return "";
            }

            // 查询catch中的方法调用的参数1的可能的信息，应使用了catch的异常对象
            MethodCallPossibleInfo catchMethodCallPossibleInfo = methodHandler4TypeAndValue.getMethodCallPossibleInfo(catchInvokeVirtualIh.getPosition());
            if (catchMethodCallPossibleInfo == null) {
                return "";
            }
            MethodCallPossibleList arg1PossibleList = catchMethodCallPossibleInfo.getPossibleInfo4Args(0);
            if (arg1PossibleList == null || arg1PossibleList.getMethodCallPossibleEntryList() == null) {
                return "";
            }
            for (MethodCallPossibleEntry arg1PossibleEntry : arg1PossibleList.getMethodCallPossibleEntryList()) {
                if (arg1PossibleEntry.getCatchExceptionStartPosition() == catchInfo.getTargetPosition()) {
                    return JavaCG2Constants.FILE_KEY_CATCH_FLAG_TRY_WITH_RESOURCE;
                }
            }
        }
        return "";
    }

    /**
     * 获取catch代码块的结束偏移量
     *
     * @param catchInfo
     * @param catchFinallyInfoMap
     * @param tryInfoMap
     * @return
     */
    private int getCatchEndPosition(CatchInfo catchInfo, Map<Integer, FinallyInfo> catchFinallyInfoMap, Map<Integer, CatchAndFinallyInfo> tryInfoMap) {
        // 优先使用finally信息获取catch代码块结束偏移量
        FinallyInfo finallyInfo = catchFinallyInfoMap.get(catchInfo.getTargetPosition());
        if (finallyInfo != null) {
            return finallyInfo.getEndPosition();
        }
        // 获取对应的try、catch、finally信息
        CatchAndFinallyInfo catchAndFinallyInfo = tryInfoMap.get(catchInfo.getFromPosition());
        List<CatchInfo> catchInfoListOfTry = catchAndFinallyInfo.getCatchInfoList();
        int catchInfoListOfTrySize = catchInfoListOfTry.size();
        if (catchInfoListOfTrySize > 1) {
            // 当前的try有多个catch，判断当前catch是第几个
            int catchIndex = findCatchInfoIndexInList(catchInfo, catchInfoListOfTry);
            if (catchIndex < catchInfoListOfTrySize - 1) {
                // 使用下一个catch获取当前catch代码块结束偏移量
                CatchInfo nextCatchInfo = catchInfoListOfTry.get(catchIndex + 1);
                return JavaCG2InstructionUtil.getInstructionPositionBefore(instructionList, nextCatchInfo.getTargetPosition());
            }
        }

        int tryEndPosition = catchInfo.getToPosition();
        // 当前的try只有一个catch，或者有多个catch且当前是最后一个catch
        // 从try的最后一个指令开始向前找到第一个goto或return类指令
        InstructionHandle tryLastIh = instructionList.findHandle(catchInfo.getToPosition());
        InstructionHandle tryLastGotoReturnIh = JavaCG2InstructionUtil.findLastIh(tryLastIh, JavaCG2InstructionUtil.GOTO_RETURN_OPCODES);
        if (tryLastGotoReturnIh == null) {
            // 从try的最后一个指令开始向前未找到goto或return类指令，尝试使用对应的try的finally代码块开始指令前一个指令的偏移量
            FinallyInfo tryFinallyInfo = catchAndFinallyInfo.getFinallyInfo();
            if (tryFinallyInfo != null) {
                return JavaCG2InstructionUtil.getInstructionPositionBefore(instructionList, tryFinallyInfo.getTargetPosition());
            }
            // 从try的最后一个指令开始向前未找到goto或return类指令，对应的try也没有finally时，使用最后一个指令作为catch代码块最后的指令
            return instructionList.getEnd().getPosition();
        }

        Instruction tryLastGotoReturnInstruction = tryLastGotoReturnIh.getInstruction();
        if (JavaCG2InstructionUtil.isReturnInstruction(tryLastGotoReturnInstruction.getOpcode())) {
            // return类指令，使用最后一个指令的偏移量作为 catch 代码块结束位置
            return instructionList.getEnd().getPosition();
        }

        // try代码块最后找到goto类指令
        GotoInstruction tryLastGotoInstruction = (GotoInstruction) tryLastGotoReturnInstruction;
        int gotoTargetPosition = tryLastGotoInstruction.getTarget().getPosition();
        if (gotoTargetPosition < tryLastGotoReturnIh.getPosition()) {
            /*
                goto往前跳转
                从goto的目标指令开始，到try代码块结束指令之间，查找if类指令，满足跳转目标指令大于try代码块结束指令的情况
             */
            InstructionHandle findIfIh = instructionList.findHandle(gotoTargetPosition);
            while (findIfIh != null && findIfIh.getPosition() <= tryEndPosition) {
                Instruction currentInstruction = findIfIh.getInstruction();
                if (currentInstruction instanceof IfInstruction) {
                    IfInstruction ifInstruction = (IfInstruction) currentInstruction;
                    if (ifInstruction.getTarget().getPosition() > tryEndPosition) {
                        // 找到跳转目标指令大于try代码块结束指令的if类指令，使用跳转目标指令的前一个指令的偏移量作为 catch 代码块结束位置
                        return ifInstruction.getTarget().getPrev().getPosition();
                    }
                }
                findIfIh = findIfIh.getNext();
            }

            /*
                goto往前跳转时，未找到向后跳转的if类指令
                从第一个catch代码块开始向后找到第一个goto指令
             */
            CatchInfo firstCatchInfo = catchInfoListOfTry.get(0);
            InstructionHandle gotoIh = JavaCG2InstructionUtil.findFirstIhBetween(instructionList, firstCatchInfo.getTargetPosition(), Integer.MAX_VALUE,
                    TypeConstants.GOTO_OPCODES);
            if (gotoIh == null) {
                // 从第一个catch代码块开始向后未找到goto指令时，使用最后一个指令作为catch代码块最后的指令
                return instructionList.getEnd().getPosition();
            }
            // 使用从第一个catch代码块开始向后找到第一个goto指令
            GotoInstruction gotoInstruction = (GotoInstruction) gotoIh.getInstruction();
            InstructionHandle gotoTargetPrevIh = gotoInstruction.getTarget().getPrev();
            if (gotoTargetPrevIh == null) {
                // goto指令的跳转目标指令的前一条指令不存在时，使用goto指令的前一条指令作为catch代码块结束位置
                return gotoIh.getPrev().getPosition();
            }
            // 使用从第一个catch代码块开始向后找到第一个goto指令的跳转目标指令的前一条指令作为catch代码块结束位置
            return gotoTargetPrevIh.getPosition();
        }

        // goto向后跳转
        InstructionHandle beforeTryLastGotoTargetIh = tryLastGotoInstruction.getTarget().getPrev();
        // 判断try最后的与catch的第一个goto跳转目标指令是否相同
        int tryLastGotoPosition = tryLastGotoInstruction.getTarget().getPosition();
        InstructionHandle catchFirstGotoIh = JavaCG2InstructionUtil.findFirstIhBetween(instructionList, catchInfo.getTargetPosition(), tryLastGotoPosition,
                TypeConstants.GOTO_OPCODES);
        if (catchFirstGotoIh != null) {
            GotoInstruction catchFirstGoto = (GotoInstruction) catchFirstGotoIh.getInstruction();
            if (catchFirstGoto.getTarget().getPosition() == tryLastGotoPosition) {
                // try最后的与catch的第一个goto跳转目标指令相同，使用catch的第一个goto指令的前一个指令作为结束指令
                return catchFirstGotoIh.getPrev().getPosition();
            }
        }
        // try代码块中跳转目标指令的前一个指令的偏移量，作为 catch 代码块结束位置
        return beforeTryLastGotoTargetIh.getPosition();
    }

    /**
     * 对于指定的指令偏移敬意，从前往后找到第一个方法调用ID
     *
     * @param startPosition
     * @param endPosition
     * @return
     */
    private int getMinCallId(int startPosition, int endPosition) {
        Integer callId = invokeInstructionPositionCallIdMap.get(startPosition);
        if (callId != null) {
            return callId;
        }
        InstructionHandle instructionHandle = instructionList.findHandle(startPosition);
        do {
            callId = invokeInstructionPositionCallIdMap.get(instructionHandle.getPosition());
            if (callId != null) {
                return callId;
            }
            instructionHandle = instructionHandle.getNext();
        } while (instructionHandle != null && instructionHandle.getPosition() <= endPosition);
        return JavaCG2Constants.METHOD_CALL_ID_MIN_BEFORE;
    }

    /**
     * 对于指定的指令偏移敬意，从前往后找到第一个方法调用ID
     *
     * @param startPosition
     * @param endPosition
     * @return
     */
    private int getMaxCallId(int startPosition, int endPosition) {
        Integer callId = invokeInstructionPositionCallIdMap.get(endPosition);
        if (callId != null) {
            return callId;
        }
        InstructionHandle instructionHandle = instructionList.findHandle(endPosition);
        do {
            callId = invokeInstructionPositionCallIdMap.get(instructionHandle.getPosition());
            if (callId != null) {
                return callId;
            }
            instructionHandle = instructionHandle.getPrev();
        } while (instructionHandle != null && instructionHandle.getPosition() >= startPosition);
        return JavaCG2Constants.METHOD_CALL_ID_MIN_BEFORE;
    }

    private int findCatchInfoIndexInList(CatchInfo catchInfo, List<CatchInfo> catchInfoList) {
        for (int i = 0; i < catchInfoList.size(); i++) {
            if (catchInfo.getTargetPosition() == catchInfoList.get(i).getTargetPosition()) {
                return i;
            }
        }
        return -1;
    }

    // 处理方法注解
    private void handleMethodAnnotations() {
        for (AnnotationEntry annotationEntry : method.getAnnotationEntries()) {
            String annotationClassName = Utility.typeSignatureToString(annotationEntry.getAnnotationType(), false);
            // 判断方法上每个注解是否存在对应的扩展类处理
            MethodAnnotationParser methodAnnotationParser = extensionsManager.getMethodAnnotationParser(annotationClassName);
            if (methodAnnotationParser == null) {
                continue;
            }

            // 使用扩展类处理方法注解
            methodAnnotationParser.parseMethodAnnotation(callerClassName, callerMethodName, callerMethodArgTypes, methodReturnType, annotationClassName, annotationEntry,
                    methodCallList);
        }
    }

    // 处理方法调用指令
    private void handleInvokeInstruction(MethodCallPossibleInfo methodCallPossibleInfo) throws IOException {
        logger.debug("处理方法调用指令 {} ({})", JavaCG2InstructionUtil.getInstructionHandlePrintInfo(ih), getSourceLine());

        Instruction invokeInstruction = ih.getInstruction();
        JavaCG2MethodInfo calleeMethodInfo = JavaCG2InstructionUtil.getCalleeMethodInfo((InvokeInstruction) invokeInstruction, cpg);
        switch (invokeInstruction.getOpcode()) {
            case Const.INVOKEVIRTUAL:
                // 处理INVOKEVIRTUAL指令
                handleINVOKEVIRTUAL((INVOKEVIRTUAL) invokeInstruction, calleeMethodInfo, methodCallPossibleInfo);
                break;
            case Const.INVOKEINTERFACE:
                // 处理INVOKEINTERFACE指令
                handleINVOKEINTERFACE((INVOKEINTERFACE) invokeInstruction, calleeMethodInfo, methodCallPossibleInfo);
                break;
            case Const.INVOKESPECIAL:
                // 处理INVOKESPECIAL指令
                handleINVOKESPECIAL((INVOKESPECIAL) invokeInstruction, calleeMethodInfo, methodCallPossibleInfo);
                break;
            case Const.INVOKESTATIC:
                // 处理INVOKESTATIC指令
                handleINVOKESTATIC((INVOKESTATIC) invokeInstruction, calleeMethodInfo, methodCallPossibleInfo);
                break;
            case Const.INVOKEDYNAMIC:
                // 处理INVOKEDYNAMIC指令
                handleINVOKEDYNAMIC((INVOKEDYNAMIC) invokeInstruction, calleeMethodInfo, methodCallPossibleInfo);
                break;
            default:
                logger.error("不会执行到此: {}", invokeInstruction.getOpcode());
                break;
        }
    }

    // 处理ATHROW指令
    private void handleATHROW(ThrowInfoList throwInfoList) throws IOException {
        int position = ih.getPosition();
        int sourceLine = getSourceLine(position);
        for (int i = 0; i < throwInfoList.size(); i++) {
            ThrowInfo throwInfo = throwInfoList.get(i);
            Integer methodThrowReturnCallId = null;
            Integer invokeInstructionPosition = throwInfo.getInvokeInstructionPosition();
            if (invokeInstructionPosition != null) {
                methodThrowReturnCallId = getInvokeInstructionCallId(invokeInstructionPosition);
            }
            JavaCG2FileUtil.write2FileWithTab(methodThrowWriter,
                    callerFullMethod,
                    String.valueOf(position),
                    String.valueOf(sourceLine),
                    String.valueOf(i),
                    throwInfo.getThrowExceptionType(),
                    throwInfo.getThrowFlag(),
                    JavaCG2Util.genStringFromInteger(throwInfo.getCatchStartPosition()),
                    throwInfo.getCatchExceptionVariableName(),
                    JavaCG2Util.genStringFromInteger(methodThrowReturnCallId));
        }
    }

    // 处理INVOKEVIRTUAL指令
    private void handleINVOKEVIRTUAL(INVOKEVIRTUAL invokevirtual, JavaCG2MethodInfo calleeMethodInfo, MethodCallPossibleInfo methodCallPossibleInfo) {
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArgTypes = calleeMethodInfo.getMethodArgumentTypes();

        // 记录线程相关的方法调用，Thread子类
        addMethodCall4ThreadStart(calleeClassName, calleeMethodName, calleeArgTypes);

        if (methodCallPossibleInfo == null) {
            // 记录方法调用信息
            addCommonMethodCall(invokevirtual, JavaCG2CallTypeEnum.CTE_RAW_INVOKE_VIRTUAL, null, calleeClassName, calleeMethodName, calleeArgTypes, null, null);
            return;
        }

        // 处理被调用类型可变的调用
        handleChangeableCalleeType(invokevirtual, false, calleeClassName, calleeMethodName, calleeArgTypes, methodCallPossibleInfo);
    }

    // 处理INVOKEINTERFACE指令
    private void handleINVOKEINTERFACE(INVOKEINTERFACE invokeinterface, JavaCG2MethodInfo calleeMethodInfo, MethodCallPossibleInfo methodCallPossibleInfo) {
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArgTypes = calleeMethodInfo.getMethodArgumentTypes();

        if (methodCallPossibleInfo == null) {
            // 记录方法调用信息
            addCommonMethodCall(invokeinterface, JavaCG2CallTypeEnum.CTE_RAW_INVOKE_INTERFACE, null, calleeClassName, calleeMethodName, calleeArgTypes, null, null);
            return;
        }

        // 处理被调用类型可变的调用
        handleChangeableCalleeType(invokeinterface, true, calleeClassName, calleeMethodName, calleeArgTypes, methodCallPossibleInfo);
    }

    // 处理INVOKESPECIAL指令
    private void handleINVOKESPECIAL(INVOKESPECIAL invokespecial, JavaCG2MethodInfo calleeMethodInfo, MethodCallPossibleInfo methodCallPossibleInfo) {
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArgTypes = calleeMethodInfo.getMethodArgumentTypes();

        // 记录线程相关的方法调用，Runnable、Callable实现类
        boolean skipRawMethodCall = addMethodCall4SpecialInit(calleeClassName, calleeMethodName, calleeArgTypes);
        if (skipRawMethodCall) {
            return;
        }

        JavaCG2CallTypeEnum callTypeEnum = JavaCG2CallTypeEnum.CTE_RAW_INVOKE_SPECIAL;
        if (!JavaCG2ClassMethodUtil.isInitMethod(calleeMethodName)
                && calleeClassName.equals(javaClass.getSuperclassName())
                && methodCallPossibleInfo != null && JavaCG2CalleeObjTypeEnum.COTE_THIS == methodCallPossibleInfo.getObjTypeEnum()) {
            /*
                满足以下所有条件时，将方法调用类型修改为代表super.方法调用：
                - 方法调用指令为INVOKESPECIAL
                - 被调用方法不是<init>
                - 被调用类为调用类的父类
                - 被调用对象是this
             */
            callTypeEnum = JavaCG2CallTypeEnum.CTE_CHILD_CALL_SUPER_SPECIAL;
        }

        // 记录方法调用信息
        addCommonMethodCallWithInfo(invokespecial, callTypeEnum, null, calleeClassName, calleeMethodName, calleeArgTypes, methodCallPossibleInfo);
    }

    // 处理INVOKESTATIC指令
    private void handleINVOKESTATIC(INVOKESTATIC invokestatic, JavaCG2MethodInfo calleeMethodInfo, MethodCallPossibleInfo methodCallPossibleInfo) {
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArgTypes = calleeMethodInfo.getMethodArgumentTypes();

        // 记录方法调用信息
        addCommonMethodCallWithInfo(invokestatic, JavaCG2CallTypeEnum.CTE_RAW_INVOKE_STATIC, null, calleeClassName, calleeMethodName, calleeArgTypes, methodCallPossibleInfo);
    }

    // 处理INVOKEDYNAMIC指令
    private void handleINVOKEDYNAMIC(INVOKEDYNAMIC invokedynamic, JavaCG2MethodInfo calleeMethodInfo, MethodCallPossibleInfo methodCallPossibleInfo) throws IOException {
        String calleeClassName = calleeMethodInfo.getClassName();
        String calleeMethodName = calleeMethodInfo.getMethodName();
        Type[] calleeArgTypes = calleeMethodInfo.getMethodArgumentTypes();

        // 记录INVOKEDYNAMIC指令对应的方法调用信息
        addCommonMethodCallWithInfo(invokedynamic, JavaCG2CallTypeEnum.CTE_RAW_INVOKE_DYNAMIC, null, calleeClassName, calleeMethodName, calleeArgTypes, methodCallPossibleInfo);

        // 判断是否为Lambda表达式
        Constant constant = cpg.getConstant(invokedynamic.getIndex());
        if (!(constant instanceof ConstantInvokeDynamic)) {
            return;
        }

        // 处理Lambda表达式
        ConstantInvokeDynamic cid = (ConstantInvokeDynamic) constant;
        // 获得JavaClass中指定下标的BootstrapMethod
        BootstrapMethod bootstrapMethod = JavaCG2BootstrapMethodUtil.getBootstrapMethod(javaClass, cid.getBootstrapMethodAttrIndex());
        if (bootstrapMethod == null) {
            logger.warn("无法找到bootstrapMethod {} {}", callerClassName, cid.getBootstrapMethodAttrIndex());
            return;
        }

        // 获得BootstrapMethod的方法信息
        JavaCG2MethodInfo bootstrapMethodInfo = JavaCG2BootstrapMethodUtil.getBootstrapMethodInfo(bootstrapMethod, javaClass);
        if (bootstrapMethodInfo == null) {
            logger.warn("无法找到bootstrapMethod的方法信息 {} {}", callerClassName, bootstrapMethod);
            return;
        }

        // 记录Lambda表达式实际的方法调用信息
        MethodCall methodCall = addCommonMethodCall(invokedynamic, JavaCG2CallTypeEnum.CTE_LAMBDA, null, bootstrapMethodInfo.getClassName(), bootstrapMethodInfo.getMethodName(),
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
            if (!JavaCG2InstructionUtil.isMethodInvokeInstruction(nextOpcode)) {
                // 向后找到非方法调用指令则结束
                break;
            }

            if (JavaCG2InstructionUtil.isMethodInvokeInstructionExcludeDynamic(nextOpcode)) {
                // 向后找到非INVOKEDYNAMIC方法调用指令，记录对应的方法
                JavaCG2MethodInfo nextCalleeMethodInfo = JavaCG2InstructionUtil.getCalleeMethodInfo((InvokeInstruction) nextInstruction, cpg);
                nextCalleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(nextCalleeMethodInfo);
                break;
            }
            nextIh = nextIh.getNext();
        }

        String calleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeArgTypes);
        if (nextCalleeFullMethod == null) {
            // 记录被调用的Lambda表达式方法信息，不包含下一个被调用方法信息
            JavaCG2FileUtil.write2FileWithTab(lambdaMethodInfoWriter, String.valueOf(methodCall.getCallId()), calleeFullMethod);
        } else {
            // 记录被调用的Lambda表达式方法信息，包含下一个被调用方法信息
            JavaCG2FileUtil.write2FileWithTab(lambdaMethodInfoWriter, String.valueOf(methodCall.getCallId()), calleeFullMethod, nextCalleeFullMethod);
        }
    }

    /**
     * 处理被调用类型可变的调用
     *
     * @param invokeInstruction      方法调用指令
     * @param isInterface            被调用对象是否为接口
     * @param calleeClassName
     * @param calleeMethodName
     * @param argTypes
     * @param methodCallPossibleInfo
     */
    private void handleChangeableCalleeType(InvokeInstruction invokeInstruction,
                                            boolean isInterface,
                                            String calleeClassName,
                                            String calleeMethodName,
                                            Type[] argTypes,
                                            MethodCallPossibleInfo methodCallPossibleInfo) {
        // 记录已处理过的被调用对象类型
        Set<String> handledCalleeTypeSet = new HashSet<>();

        MethodCallPossibleList methodCallPossibleList4Object = methodCallPossibleInfo.getPossibleInfo4Object();
        if (methodCallPossibleList4Object != null) {
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
                    if (JavaCG2Util.isCollectionEmpty(springBeanFieldTypeList)) {
                        continue;
                    }
                    for (String springBeanFieldType : springBeanFieldTypeList) {
                        // 尝试添加方法调用信息，使用对应的被调用类型
                        tryAddMethodCallWithType(invokeInstruction, isInterface, true, handledCalleeTypeSet, calleeClassName, springBeanFieldType, calleeMethodName, argTypes,
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
                        tryAddMethodCallWithType(invokeInstruction, isInterface, false, handledCalleeTypeSet, calleeClassName, type, calleeMethodName, argTypes,
                                methodCallPossibleInfo);
                    }
                }
            }
        }

        if (handledCalleeTypeSet.isEmpty()) {
            // 未添加与方法调用指令中被调用类不同类型的调用信息，使用方法调用指令中被调用类进行添加
            // 获取实际的被调用类型
            JavaCG2CallTypeEnum callTypeEnum = isInterface ? JavaCG2CallTypeEnum.CTE_RAW_INVOKE_INTERFACE : JavaCG2CallTypeEnum.CTE_RAW_INVOKE_VIRTUAL;

            // 记录方法调用信息
            addCommonMethodCallWithInfo(invokeInstruction, callTypeEnum, null, calleeClassName, calleeMethodName, argTypes, methodCallPossibleInfo);
        }
    }

    /**
     * 获取实际的被调用类型
     *
     * @param isInterface  被调用对象是否为接口
     * @param isSpringBean 被调用对象是否为Spring Bean
     * @return
     */
    private JavaCG2CallTypeEnum chooseActualCallType(boolean isInterface, boolean isSpringBean) {
        // 有替换被调用对象的类型
        if (isSpringBean) {
            return isInterface ? JavaCG2CallTypeEnum.CTE_SPRING_BEAN_ACTUAL_INTERFACE : JavaCG2CallTypeEnum.CTE_SPRING_BEAN_ACTUAL_CLASS;
        }
        return isInterface ? JavaCG2CallTypeEnum.CTE_ACTUAL_INTERFACE : JavaCG2CallTypeEnum.CTE_ACTUAL_CLASS;
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
     * @param argTypes
     * @param methodCallPossibleInfo
     */
    private void tryAddMethodCallWithType(InvokeInstruction invokeInstruction,
                                          boolean isInterface,
                                          boolean isSpringBean,
                                          Set<String> handledCalleeTypeSet,
                                          String calleeClassName,
                                          String calleeTypeRuntime,
                                          String calleeMethodName,
                                          Type[] argTypes,
                                          MethodCallPossibleInfo methodCallPossibleInfo) {
        if (handledCalleeTypeSet.contains(calleeTypeRuntime) ||
                StringUtils.equals(calleeClassName, calleeTypeRuntime) ||
                JavaCG2ByteCodeUtil.isNullType(calleeTypeRuntime) ||
                JavaCG2ClassMethodUtil.isObjectClass(calleeTypeRuntime)) {
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
        JavaCG2CallTypeEnum callTypeEnum = chooseActualCallType(isInterface, isSpringBean);

        // 记录方法调用信息
        addCommonMethodCallWithInfo(invokeInstruction, callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, argTypes, methodCallPossibleInfo);
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
     * @param argTypes               被调用方法参数类型数组
     * @param methodCallPossibleInfo 方法调用可能的信息
     */
    private void addCommonMethodCallWithInfo(InvokeInstruction invokeInstruction,
                                             JavaCG2CallTypeEnum callTypeEnum,
                                             String calleeTypeRuntime,
                                             String calleeClassName,
                                             String calleeMethodName,
                                             Type[] argTypes,
                                             MethodCallPossibleInfo methodCallPossibleInfo) {
        if (methodCallPossibleInfo == null) {
            addCommonMethodCall(invokeInstruction, callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, argTypes, null, null);
            return;
        }

        MethodCall methodCall = addCommonMethodCall(invokeInstruction, callTypeEnum, calleeTypeRuntime, calleeClassName, calleeMethodName, argTypes, null,
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
     * @param calleeArgTypes    被调用方法参数类型数组
     * @param calleeReturnType  被调用方法返回类型
     * @param objTypeEnum       被调用对象类型
     */
    private MethodCall addCommonMethodCall(InvokeInstruction invokeInstruction,
                                           JavaCG2CallTypeEnum callTypeEnum,
                                           String calleeTypeRuntime,
                                           String calleeClassName,
                                           String calleeMethodName,
                                           Type[] calleeArgTypes,
                                           Type calleeReturnType,
                                           JavaCG2CalleeObjTypeEnum objTypeEnum) {
        if (JavaCG2Util.checkSkipClass(calleeClassName, javaCG2ConfInfo.getNeedHandlePackageSet())) {
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
                        actualReturnType = JavaCG2InstructionUtil.getTypeString(checkcast, cpg);
                    }
                }
            }
        }

        // 若运行时的被调用类型非空则使用，若为空则使用方法调用指令中的被调用类名
        MethodCall methodCall = new MethodCall();
        methodCall.setCallId(genNewCallId());
        methodCall.setCallerClassName(callerClassName);
        methodCall.setCallerMethodName(callerMethodName);
        methodCall.setCallerMethodArgTypes(callerMethodArgTypes);
        methodCall.setCallerReturnType(methodReturnType);
        methodCall.setMethodCallType(callTypeEnum);
        methodCall.setCalleeClassName(calleeTypeRuntime != null ? calleeTypeRuntime : calleeClassName);
        methodCall.setCalleeMethodName(calleeMethodName);
        methodCall.setCalleeMethodArgTypes(JavaCG2ClassMethodUtil.getArgTypeStr(calleeArgTypes));
        methodCall.setCallerSourceLine(getSourceLine());
        methodCall.setObjTypeEnum(objTypeEnum);
        // 假如指定的返回类型calleeReturnType非空则使用，若为空则使用通过方法调用指令获取的方法返回类型
        methodCall.setRawReturnType(calleeReturnType != null ? calleeReturnType.toString() : rawReturnType);
        methodCall.setActualReturnType(actualReturnType);
        methodCall.setArgTypes(calleeArgTypes);
        methodCallList.addMethodCall(methodCall);

        return methodCall;
    }

    // 添加其他方法调用关系
    private void addOtherMethodCall(String callerClassName,
                                    String callerMethodName,
                                    String callerMethodArgTypes,
                                    String callerReturnType,
                                    JavaCG2CallTypeEnum methodCallType,
                                    String calleeClassName,
                                    String calleeMethodName,
                                    String calleeMethodArgTypes,
                                    String calleeReturnType,
                                    int callerSourceLine) {
        if (JavaCG2Util.checkSkipClass(calleeClassName, javaCG2ConfInfo.getNeedHandlePackageSet())) {
            return;
        }

        MethodCall methodCall = new MethodCall();
        methodCall.setCallId(genNewCallId());
        methodCall.setCallerClassName(callerClassName);
        methodCall.setCallerMethodName(callerMethodName);
        methodCall.setCallerMethodArgTypes(callerMethodArgTypes);
        methodCall.setCallerSourceLine(callerSourceLine);
        methodCall.setCallerReturnType(callerReturnType);
        methodCall.setMethodCallType(methodCallType);
        methodCall.setCalleeClassName(calleeClassName);
        methodCall.setCalleeMethodName(calleeMethodName);
        methodCall.setCalleeMethodArgTypes(calleeMethodArgTypes);
        methodCall.setRawReturnType(calleeReturnType);
        methodCallList.addMethodCall(methodCall);
    }

    /**
     * 记录特殊的构造函数调用，包括Runnable、Callable实现类、TransactionTemplate相关的类等
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @param argTypes
     * @return true: 不记录原始的方法调用类型，false: 记录原始的方法调用类型
     */
    private boolean addMethodCall4SpecialInit(String calleeClassName, String calleeMethodName, Type[] argTypes) {
        if (!JavaCG2ClassMethodUtil.isInitMethod(calleeMethodName)) {
            // 记录原始的方法调用类型
            return false;
        }

        boolean skipRawMethodCall = false;
        String calleeMethodArgTypes = JavaCG2ClassMethodUtil.getArgTypeStr(argTypes);

        // 处理Runnable实现类，run方法返回类型为void
        if (handleSpecialInitMethod(runnableImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgTypes, JavaCG2CallTypeEnum.CTE_RUNNABLE_INIT_RUN1,
                JavaCG2CallTypeEnum.CTE_RUNNABLE_INIT_RUN2, JavaCG2CommonNameConstants.METHOD_RUNNABLE_RUN, JavaCG2Constants.EMPTY_METHOD_ARGS,
                JavaCG2CommonNameConstants.RETURN_TYPE_VOID)) {
            skipRawMethodCall = true;
        }

        // 处理Callable实现类，call方法返回类型不固定
        if (handleSpecialInitMethod(callableImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgTypes, JavaCG2CallTypeEnum.CTE_CALLABLE_INIT_CALL1,
                JavaCG2CallTypeEnum.CTE_CALLABLE_INIT_CALL2, JavaCG2CommonNameConstants.METHOD_CALLABLE_CALL, JavaCG2Constants.EMPTY_METHOD_ARGS, "")) {
            skipRawMethodCall = true;
        }

        // 处理TransactionCallback实现类，doInTransaction方法返回类型不固定
        if (handleSpecialInitMethod(transactionCallbackImplClassMap, calleeClassName, calleeMethodName, calleeMethodArgTypes, JavaCG2CallTypeEnum.CTE_TX_CALLBACK_INIT_CALL1,
                JavaCG2CallTypeEnum.CTE_TX_CALLBACK_INIT_CALL2, JavaCG2CommonNameConstants.METHOD_DO_IN_TRANSACTION, JavaCG2CommonNameConstants.ARGS_TRANSACTION_STATUS, "")) {
            skipRawMethodCall = true;
        }

        // 处理TransactionCallbackWithoutResult实现类，doInTransactionWithoutResult方法返回类型为void
        if (handleSpecialInitMethod(transactionCallbackWithoutResultChildClassMap, calleeClassName, calleeMethodName, calleeMethodArgTypes,
                JavaCG2CallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL1, JavaCG2CallTypeEnum.CTE_TX_CALLBACK_WR_INIT_CALL2,
                JavaCG2CommonNameConstants.METHOD_DO_IN_TRANSACTION_WITHOUT_RESULT, JavaCG2CommonNameConstants.ARGS_TRANSACTION_STATUS, JavaCG2CommonNameConstants.RETURN_TYPE_VOID)) {
            skipRawMethodCall = true;
        }
        return skipRawMethodCall;
    }

    // 处理<init>方法，需要增加调用其他方法的情况
    private boolean handleSpecialInitMethod(Map<String, Boolean> map,
                                            String calleeClassName,
                                            String calleeMethodName,
                                            String calleeMethodArgTypes,
                                            JavaCG2CallTypeEnum callTypeEnum1,
                                            JavaCG2CallTypeEnum callTypeEnum2,
                                            String addedCalleeMethodName,
                                            String addedCalleeMethodArgTypes,
                                            String addedCalleeReturnType) {
        Boolean recorded = map.get(calleeClassName);
        if (recorded == null) {
            return false;
        }

        // 记录其他方法调用对应类的<init>方法
        addOtherMethodCall(callerClassName, callerMethodName, callerMethodArgTypes, methodReturnType, callTypeEnum1, calleeClassName, calleeMethodName, calleeMethodArgTypes,
                JavaCG2CommonNameConstants.RETURN_TYPE_VOID, getSourceLine());

        if (Boolean.FALSE.equals(recorded)) {
            // 对应类的<init>方法调用需要增加的方法，<init>方法返回类型为void
            addOtherMethodCall(calleeClassName, calleeMethodName, calleeMethodArgTypes, JavaCG2CommonNameConstants.RETURN_TYPE_VOID, callTypeEnum2, calleeClassName,
                    addedCalleeMethodName, addedCalleeMethodArgTypes, addedCalleeReturnType, JavaCG2Constants.DEFAULT_LINE_NUMBER);
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
     * @param argTypes
     */
    private void addMethodCall4ThreadStart(String calleeClassName, String calleeMethodName, Type[] argTypes) {
        if (!JavaCG2CommonNameConstants.METHOD_NAME_START.equals(calleeMethodName) || argTypes.length > 0) {
            // 被调用方法不是start()，返回
            return;
        }

        // 处理Thread子类
        if (!Boolean.FALSE.equals(threadChildClassMap.get(calleeClassName))) {
            return;
        }

        String calleeMethodArgTypes = JavaCG2ClassMethodUtil.getArgTypeStr(argTypes);
        // 记录Thread子类的start方法调用run方法（以上Map的value等于FALSE时，代表当前类为Thread的子类，且start()方法调用run()方法未添加过）
        // Thead类的start()、run()方法返回类型都是void
        addOtherMethodCall(calleeClassName, calleeMethodName, calleeMethodArgTypes, JavaCG2CommonNameConstants.RETURN_TYPE_VOID, JavaCG2CallTypeEnum.CTE_THREAD_START_RUN,
                calleeClassName, "run", JavaCG2Constants.EMPTY_METHOD_ARGS, JavaCG2CommonNameConstants.RETURN_TYPE_VOID, JavaCG2Constants.DEFAULT_LINE_NUMBER);
        // 避免start()方法调用run()方法被添加多次
        threadChildClassMap.put(calleeClassName, Boolean.TRUE);
    }

    // 处理方法调用可能的信息
    private void handleMethodCallPossibleInfo(MethodCall methodCall, MethodCallPossibleInfo methodCallPossibleInfo) throws IOException {
        if (methodCallPossibleInfo == null) {
            return;
        }

        // 记录方法调用可能的信息，被调用对象
        recordMethodCallPossibleInfo(methodCall.getCallId(), JavaCG2Constants.METHOD_CALL_OBJECT_SEQ, null, methodCallPossibleInfo.getPossibleInfo4Object());

        Type[] argTypes = methodCall.getArgTypes();
        // 记录方法调用可能的信息，参数
        for (int i = 0; i < methodCallPossibleInfo.getPossibleInfoNum4Args(); i++) {
            String argType = argTypes[i].toString();
            recordMethodCallPossibleInfo(methodCall.getCallId(), JavaCG2Constants.METHOD_CALL_ARGUMENTS_START_SEQ + i, argType,
                    methodCallPossibleInfo.getPossibleInfo4Args(i));
        }
    }

    // 记录方法调用可能的信息
    private void recordMethodCallPossibleInfo(int methodCallId, int argSeq, String argType, MethodCallPossibleList methodCallPossibleList) throws IOException {
        if (methodCallPossibleList == null) {
            return;
        }

        List<MethodCallPossibleEntry> methodCallPossibleEntryList = methodCallPossibleList.getMethodCallPossibleEntryList();
        if (JavaCG2Util.isCollectionEmpty(methodCallPossibleEntryList)) {
            return;
        }

        String arrayElementFlag = JavaCG2YesNoEnum.parseStrValue(methodCallPossibleList.isArrayElement());
        StringBuilder stringBuilder = new StringBuilder();
        JavaCG2Counter typeCounter = new JavaCG2Counter(-1);
        for (int seq = 0; seq < methodCallPossibleEntryList.size(); seq++) {
            MethodCallPossibleEntry methodCallPossibleEntry = methodCallPossibleEntryList.get(seq);
            // 记录方法调用可能的类型，类型的序号可能比以上list的元素数量多，需要每次累计
            recordStringMethodCallPossibleType(stringBuilder, methodCallPossibleEntry, methodCallId, argSeq, typeCounter, arrayElementFlag);

            // 处理方法调用可能的值
            recordStringMethodCallPossibleValue(stringBuilder, methodCallPossibleEntry, methodCallId, argSeq, argType, seq, arrayElementFlag);


            // 处理方法调用可能的被调用静态变量
            StaticFieldTypeAndName staticField = methodCallPossibleEntry.getStaticField();
            if (staticField != null) {
                recordStringMethodCallPossibleInfo(stringBuilder, staticField.getClassAndFieldName(), methodCallId, argSeq,
                        JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD, seq, arrayElementFlag, "");
                JavaCG2FileUtil.write2FileWithTab(methodCallStaticFieldWriter,
                        String.valueOf(methodCallId),
                        String.valueOf(argSeq),
                        String.valueOf(seq),
                        staticField.getClassName(),
                        staticField.getFieldName(),
                        staticField.getFieldType(),
                        callerFullMethod);
            }

            // 处理被调用对象或参数是静态字段方法返回值的可能信息
            recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleEntry.getStaticFieldMethodCall(), methodCallId, argSeq,
                    JavaCG2MethodCallInfoTypeEnum.MCIT_STATIC_FIELD_METHOD_CALL, seq, arrayElementFlag, "");

            // 处理被调用对象或参数的字段名称可能信息
            FieldTypeAndName nonStaticField = methodCallPossibleEntry.getNonStaticField();
            if (nonStaticField != null) {
                recordStringMethodCallPossibleInfo(stringBuilder, nonStaticField.getFieldName(), methodCallId, argSeq,
                        JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_FIELD, seq, arrayElementFlag, "");
            }

            // 处理被调用对象或参数的变量名称可能信息
            recordStringMethodCallPossibleInfo(stringBuilder, methodCallPossibleEntry.getNameOfVariable(), methodCallId, argSeq,
                    JavaCG2MethodCallInfoTypeEnum.MCIT_NAME_OF_VARIABLE, seq, arrayElementFlag, "");

            // 处理被调用对象或参数的方法调用返回call_id可能信息
            String methodCallReturnFullMethod = methodCallPossibleEntry.getMethodCallReturnFullMethod();
            if (StringUtils.isNotBlank(methodCallReturnFullMethod) && !JavaCG2Util.checkSkipClass(methodCallReturnFullMethod, javaCG2ConfInfo.getNeedHandlePackageSet())) {
                Integer methodCallReturnInstructionPosition = methodCallPossibleEntry.getMethodCallReturnInstructionPosition();
                if (methodCallReturnInstructionPosition != null) {
                    // 根据方法调用指令位置查找对应的call_id
                    Integer methodCallReturnCallId = getInvokeInstructionCallId(methodCallReturnInstructionPosition);
                    if (methodCallReturnCallId != methodCallId) {
                        // 仅当当前call_id与被调用对象或参数的方法调用返回call_id不同时才记录
                        recordStringMethodCallPossibleInfo(stringBuilder, String.valueOf(methodCallReturnCallId), methodCallId, argSeq,
                                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID, seq, arrayElementFlag, "");
                        // 记录方法调用使用方法调用返回值
                        JavaCG2FileUtil.write2FileWithTab(methodCallMethodCallReturnWriter,
                                String.valueOf(methodCallId),
                                String.valueOf(argSeq),
                                String.valueOf(seq),
                                String.valueOf(arrayElementFlag),
                                String.valueOf(methodCallReturnCallId),
                                methodCallReturnFullMethod);
                    }
                }
            }

            // 处理被调用对象或参数的方法参数序号可能信息
            Integer methodArgSeq = methodCallPossibleEntry.getMethodArgSeq();
            if (methodArgSeq != null) {
                recordStringMethodCallPossibleInfo(stringBuilder, String.valueOf(methodArgSeq), methodCallId, argSeq,
                        JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ, seq, arrayElementFlag, "");
            }

            // 处理被调用对象或参数等值转换前的方法调用返回call_id可能信息
            String methodCallReturnFullMethodEQC = methodCallPossibleEntry.getMethodCallReturnFullMethodEQC();
            if (StringUtils.isNotBlank(methodCallReturnFullMethodEQC) && !JavaCG2Util.checkSkipClass(methodCallReturnFullMethodEQC, javaCG2ConfInfo.getNeedHandlePackageSet())) {
                Integer methodCallReturnInstructionPositionEQC = methodCallPossibleEntry.getMethodCallReturnInstructionPositionEQC();
                if (methodCallReturnInstructionPositionEQC != null) {
                    // 根据方法调用指令位置查找对应的call_id
                    Integer methodCallReturnCallIdEQC = getInvokeInstructionCallId(methodCallReturnInstructionPositionEQC);
                    if (methodCallReturnCallIdEQC != methodCallId) {
                        // 仅当当前call_id与被调用对象或参数的方法调用返回call_id不同时才记录
                        recordStringMethodCallPossibleInfo(stringBuilder, String.valueOf(methodCallReturnCallIdEQC), methodCallId, argSeq,
                                JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CALL_RETURN_CALL_ID_EQC, seq, arrayElementFlag, "");
                    }
                }
            }

            // 处理被调用对象或参数等值转换前的方法参数序号可能信息
            Integer methodArgSeqEQC = methodCallPossibleEntry.getMethodArgSeqEQC();
            if (methodArgSeqEQC != null) {
                recordStringMethodCallPossibleInfo(stringBuilder, String.valueOf(methodArgSeqEQC), methodCallId, argSeq,
                        JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_ARG_SEQ_EQC, seq, arrayElementFlag, "");
            }

            // 处理被调用对象或参数的catch异常对象对应的catch代码块开始指令偏移量可能信息
            Integer catchExceptionStartPosition = methodCallPossibleEntry.getCatchExceptionStartPosition();
            if (catchExceptionStartPosition != null) {
                recordStringMethodCallPossibleInfo(stringBuilder, String.valueOf(catchExceptionStartPosition), methodCallId, argSeq,
                        JavaCG2MethodCallInfoTypeEnum.MCIT_METHOD_CATCH_EXCEPTION_FROM_OFFSET, seq, arrayElementFlag, "");
            }
        }
        JavaCG2FileUtil.write2FileNoLF(methodCallInfoWriter, stringBuilder.toString());
    }

    // 记录方法调用可能的类型
    private void recordStringMethodCallPossibleType(StringBuilder stringBuilder, MethodCallPossibleEntry methodCallPossibleEntry, int methodCallId, int argSeq,
                                                    JavaCG2Counter typeCounter, String arrayElementFlag) {
        if (useSpringBeanByAnnotationHandler.hasUseSpringBean()) {
            // 涉及Spring Bean，获取被调用对象可能的非静态字段名
            FieldTypeAndName nonStaticField = methodCallPossibleEntry.getNonStaticField();
            if (nonStaticField != null) {
                // 获取指定类指定字段对应的Spring Bean类型
                List<String> springBeanFieldTypeList = useSpringBeanByAnnotationHandler.getSpringBeanTypeList(callerClassName, nonStaticField.getFieldName());
                if (!JavaCG2Util.isCollectionEmpty(springBeanFieldTypeList)) {
                    for (String springBeanFieldType : springBeanFieldTypeList) {
                        // 记录信息
                        recordStringMethodCallPossibleInfo(stringBuilder, springBeanFieldType, methodCallId, argSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE,
                                typeCounter.addAndGet(), arrayElementFlag, "");
                    }
                    return;
                }
            }
        }

        // 处理方法调用可能的类型
        String type = methodCallPossibleEntry.getType();
        if (type != null) {
            recordStringMethodCallPossibleInfo(stringBuilder, type, methodCallId, argSeq, JavaCG2MethodCallInfoTypeEnum.MCIT_TYPE, typeCounter.addAndGet(),
                    arrayElementFlag, "");
        }
    }

    // 记录方法调用可能的信息
    private void recordStringMethodCallPossibleInfo(StringBuilder stringBuilder, String data, int methodCallId, int argSeq, JavaCG2MethodCallInfoTypeEnum type, int seq,
                                                    String arrayElementFlag, String valueType) {
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
        String appendedData = JavaCG2FileUtil.appendFileColumn(
                String.valueOf(methodCallId),
                String.valueOf(argSeq),
                String.valueOf(seq),
                type.getType(),
                arrayElementFlag,
                valueType,
                data,
                callerFullMethod
        );
        stringBuilder.append(appendedData).append(JavaCG2Constants.NEW_LINE);
    }

    // 处理方法调用可能的值
    private void recordStringMethodCallPossibleValue(StringBuilder stringBuilder, MethodCallPossibleEntry methodCallPossibleEntry, int methodCallId, int argSeq, String argType,
                                                     int seq, String arrayElementFlag) {
        Object value = methodCallPossibleEntry.getValue();
        if (value == null) {
            return;
        }

        JavaCG2MethodCallInfoTypeEnum type = JavaCG2MethodCallInfoTypeEnum.MCIT_VALUE;
        String strValue;
        if (value instanceof String) {
            // 参数值类型为String
            strValue = (String) value;
        } else {
            strValue = value.toString();
        }
        // 假如值中包含可能导致文件解析时格式不符合预期的字符，则需要进行base64编码
        if (StringUtils.containsAny(strValue, "\r", "\n", "\t")) {
            strValue = JavaCG2Util.base64Encode(strValue);
            type = JavaCG2MethodCallInfoTypeEnum.MCIT_BASE64_VALUE;
        }
        // 值的类型，若方法参数类型为Object则使用值的类型
        String valueType;
        if (StringUtils.equalsAny(argType, JavaCG2CommonNameConstants.CLASS_NAME_OBJECT, JavaCG2CommonNameConstants.CLASS_NAME_CHAR_SEQUENCE)) {
            // 假如方法参数类型为Object或CharSequence（StringUtils:indexOf(java.lang.CharSequence），则使用参数对象的类型
            valueType = methodCallPossibleEntry.getValueType();
        } else {
            valueType = argType;
        }
        // 记录可能的值
        recordStringMethodCallPossibleInfo(stringBuilder, strValue, methodCallId, argSeq, type, seq, arrayElementFlag, valueType);
    }

    /**
     * 生成新的方法调用ID
     *
     * @return
     */
    private int genNewCallId() {
        int newCallId = callIdCounter.addAndGet();
        if (parseMethodCallTypeValueFlag) {
            // 记录方法调用指令位置与call_id对应关系
            invokeInstructionPositionCallIdMap.put(ih.getPosition(), newCallId);
        }
        return newCallId;
    }

    /**
     * 根据方法调用指令位置查找对应的call_id
     *
     * @param invokeInstructionPosition
     * @return
     */
    private Integer getInvokeInstructionCallId(Integer invokeInstructionPosition) {
        Integer callId = invokeInstructionPositionCallIdMap.get(invokeInstructionPosition);
        if (callId == null) {
            throw new JavaCG2RuntimeException("根据方法调用指令位置未查找到对应的call_id: " + invokeInstructionPosition);
        }
        return callId;
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

    public void setMethodArgAnnotationWriter(Writer methodArgAnnotationWriter) {
        this.methodArgAnnotationWriter = methodArgAnnotationWriter;
    }

    public void setMethodLineNumberWriter(Writer methodLineNumberWriter) {
        this.methodLineNumberWriter = methodLineNumberWriter;
    }

    public void setMethodCallInfoWriter(Writer methodCallInfoWriter) {
        this.methodCallInfoWriter = methodCallInfoWriter;
    }

    public void setMethodCallMethodCallReturnWriter(Writer methodCallMethodCallReturnWriter) {
        this.methodCallMethodCallReturnWriter = methodCallMethodCallReturnWriter;
    }

    public void setMethodCallStaticFieldWriter(Writer methodCallStaticFieldWriter) {
        this.methodCallStaticFieldWriter = methodCallStaticFieldWriter;
    }

    public void setMethodReturnArgSeqWriter(Writer methodReturnArgSeqWriter) {
        this.methodReturnArgSeqWriter = methodReturnArgSeqWriter;
    }

    public void setMethodReturnCallIdWriter(Writer methodReturnCallIdWriter) {
        this.methodReturnCallIdWriter = methodReturnCallIdWriter;
    }

    public void setMethodCatchWriter(Writer methodCatchWriter) {
        this.methodCatchWriter = methodCatchWriter;
    }

    public void setMethodFinallyWriter(Writer methodFinallyWriter) {
        this.methodFinallyWriter = methodFinallyWriter;
    }

    public void setMethodThrowWriter(Writer methodThrowWriter) {
        this.methodThrowWriter = methodThrowWriter;
    }

    public void setStaticFinalFieldMethodCallIdWriter(Writer staticFinalFieldMethodCallIdWriter) {
        this.staticFinalFieldMethodCallIdWriter = staticFinalFieldMethodCallIdWriter;
    }

    public void setFieldRelationshipWriter(Writer fieldRelationshipWriter) {
        this.fieldRelationshipWriter = fieldRelationshipWriter;
    }

    public void setLastJarNum(int lastJarNum) {
        this.lastJarNum = lastJarNum;
    }

    public void setExistsSameMethodNameAndArgs(boolean existsSameMethodNameAndArgs) {
        this.existsSameMethodNameAndArgs = existsSameMethodNameAndArgs;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }

    public void setStaticFinalFieldNameTypeMap(Map<String, String> staticFinalFieldNameTypeMap) {
        this.staticFinalFieldNameTypeMap = staticFinalFieldNameTypeMap;
    }
}
