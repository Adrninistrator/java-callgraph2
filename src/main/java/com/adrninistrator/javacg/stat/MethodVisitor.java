package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.CallIdCounter;
import com.adrninistrator.javacg.dto.MethodCallDto;
import com.adrninistrator.javacg.dto.MethodInfo;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.EmptyVisitor;
import org.apache.bcel.generic.*;

import java.util.*;

// 处理Method对象
public class MethodVisitor extends EmptyVisitor {

    private static final String OTHER_METHOD_CALL_FORMAT = JavaCGConstants.FILE_KEY_METHOD_PREFIX + "%d %s:%s%s (%s)%s:%s";

    private JavaClass visitedClass;
    private MethodGen mg;
    private ConstantPoolGen cpg;
    private String format;
    private List<MethodCallDto> methodCalls = new ArrayList<>();
    private LineNumberTable lineNumberTable;
    private InstructionHandle ih;
    private Map<String, Set<String>> calleeMethodMapGlobal;
    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> threadChildClassMap;
    private Map<String, Set<String>> methodAnnotationMap;
    private CallIdCounter callIdCounter;
    private List<CustomCodeParserInterface> customCodeParserList;

    private boolean recordAll = false;

    public MethodVisitor(MethodGen m, JavaClass jc) {
        visitedClass = jc;
        mg = m;
        cpg = mg.getConstantPool();

        lineNumberTable = mg.getLineNumberTable(cpg);
    }

    public void setCalleeMethodMapGlobal(Map<String, Set<String>> calleeMethodMapGlobal) {
        this.calleeMethodMapGlobal = calleeMethodMapGlobal;
    }

    public void setRunnableImplClassMap(Map<String, Boolean> runnableImplClassMap) {
        this.runnableImplClassMap = runnableImplClassMap;
    }

    public void setCallableImplClassMap(Map<String, Boolean> callableImplClassMap) {
        this.callableImplClassMap = callableImplClassMap;
    }

    public void setThreadChildClassMap(Map<String, Boolean> threadChildClassMap) {
        this.threadChildClassMap = threadChildClassMap;
    }

    public void setMethodAnnotationMap(Map<String, Set<String>> methodAnnotationMap) {
        this.methodAnnotationMap = methodAnnotationMap;
    }

    public void setCallIdCounter(CallIdCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setCustomCodeParserList(List<CustomCodeParserInterface> customCodeParserList) {
        this.customCodeParserList = customCodeParserList;
    }

    public void setRecordAll(boolean recordAll) {
        this.recordAll = recordAll;
    }

    public void beforeStart() {
        String fullMethod = visitedClass.getClassName() + ":" + mg.getName() + JavaCGUtil.getArgListStr(mg.getArgumentTypes());

        handleAnnotationName(fullMethod);

        format = JavaCGConstants.FILE_KEY_METHOD_PREFIX + "%d " + fullMethod + " " + "(%s)%s:%s%s";
    }

    private void handleAnnotationName(String fullMethod) {
        AnnotationEntryGen[] annotationEntryGens = mg.getAnnotationEntries();
        if (annotationEntryGens == null || annotationEntryGens.length == 0) {
            return;
        }

        Set<String> annotationNameSet = methodAnnotationMap.get(fullMethod);
        if (annotationNameSet != null) {
            return;
        }

        annotationNameSet = new HashSet<>();
        for (AnnotationEntryGen annotationEntryGen : annotationEntryGens) {
            String annotationName = getAnnotationName(annotationEntryGen.getTypeName());
            annotationNameSet.add(annotationName);
        }
        methodAnnotationMap.put(fullMethod, annotationNameSet);
    }

    private String getAnnotationName(String origName) {
        String tmpName;
        if (origName.startsWith("L") && origName.endsWith(";")) {
            tmpName = origName.substring(1, origName.length() - 1);
        } else {
            tmpName = origName;
        }
        return tmpName.replace("/", ".");
    }

    public List<MethodCallDto> start() {
        if (mg.isAbstract() || mg.isNative()) {
            return Collections.emptyList();
        }

        for (ih = mg.getInstructionList().getStart(); ih != null; ih = ih.getNext()) {
            Instruction i = ih.getInstruction();

            if (i instanceof InvokeInstruction) {
                i.accept(this);
            }
        }
        return methodCalls;
    }

    @Override
    public void visitINVOKEVIRTUAL(INVOKEVIRTUAL i) {
        addMethodCalls("M", i.getReferenceType(cpg).toString(), i.getMethodName(cpg), i.getArgumentTypes(cpg));
    }

    @Override
    public void visitINVOKEINTERFACE(INVOKEINTERFACE i) {
        addMethodCalls("I", i.getReferenceType(cpg).toString(), i.getMethodName(cpg), i.getArgumentTypes(cpg));
    }

    @Override
    public void visitINVOKESPECIAL(INVOKESPECIAL i) {
        addMethodCalls("O", i.getReferenceType(cpg).toString(), i.getMethodName(cpg), i.getArgumentTypes(cpg));
    }

    @Override
    public void visitINVOKESTATIC(INVOKESTATIC i) {
        addMethodCalls("S", i.getReferenceType(cpg).toString(), i.getMethodName(cpg), i.getArgumentTypes(cpg));
    }

    @Override
    public void visitINVOKEDYNAMIC(INVOKEDYNAMIC i) {
        addMethodCalls("D", i.getType(cpg).toString(), i.getMethodName(cpg), i.getArgumentTypes(cpg));

        Constant constant = cpg.getConstant(i.getIndex());
        if (!(constant instanceof ConstantInvokeDynamic)) {
            return;
        }

        // 处理Lambda表达式
        ConstantInvokeDynamic cid = (ConstantInvokeDynamic) constant;
        // 获得JavaClass中指定下标的BootstrapMethod
        BootstrapMethod bootstrapMethod = JavaCGUtil.getBootstrapMethod(visitedClass, cid.getBootstrapMethodAttrIndex());
        if (bootstrapMethod == null) {
            System.err.println("### 无法找到bootstrapMethod " + visitedClass.getClassName() + " " + cid.getBootstrapMethodAttrIndex());
            return;
        }

        // 获得BootstrapMethod的方法信息
        MethodInfo bootstrapMethodInfo = JavaCGUtil.getBootstrapMethodInfo(bootstrapMethod, visitedClass);
        if (bootstrapMethodInfo == null) {
            System.err.println("### 无法找到bootstrapMethod的方法信息 " + visitedClass.getClassName() + " " + bootstrapMethod);
            return;
        }

        String callType = bootstrapMethodInfo.getMethodName().startsWith(JavaCGConstants.FLAG_LAMBDA) ? CallTypeEnum.CTE_LM.getType() : CallTypeEnum.CTE_ST.getType();
        addMethodCalls(callType, bootstrapMethodInfo.getClassName(), bootstrapMethodInfo.getMethodName(), bootstrapMethodInfo.getMethodArgumentTypes());
    }

    // 记录方法调用信息
    private void addMethodCalls(String type, String calleeClassName, String calleeMethodName, Type[] arguments) {
        // 添加被调用方法信息
        String calleeMethodArgs = JavaCGUtil.getArgListStr(arguments);

        if (!recordAll && !calleeClassName.startsWith("java.")) {
            // 记录非java.包中类被调用的方法
            Set<String> calleeMethodWithArgsSet = calleeMethodMapGlobal.computeIfAbsent(calleeClassName, k -> new HashSet<>());
            calleeMethodWithArgsSet.add(calleeMethodName + calleeMethodArgs);
        }

        // 判断是否需要记录线程相关的方法调用
        if (addMethodCall4Thread(calleeClassName, calleeMethodName, calleeMethodArgs)) {
            return;
        }

        int callId = callIdCounter.addAndGet();
        String methodCall = String.format(format, callId, type, calleeClassName, calleeMethodName, calleeMethodArgs);
        MethodCallDto methodCallDto = MethodCallDto.genInstance(methodCall, getSourceLine());
        methodCalls.add(methodCallDto);

        // 调用自定义接口实现类的方法
        for (CustomCodeParserInterface customCodeParser : customCodeParserList) {
            customCodeParser.handleMethodCall(callId, calleeClassName, calleeMethodName, arguments, ih, mg);
        }
    }

    /**
     * 记录线程相关的方法调用
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @param calleeMethodArgs
     * @return true: 不记录原始的方法调用类型，false: 记录原始的方法调用类型
     */
    private boolean addMethodCall4Thread(String calleeClassName, String calleeMethodName, String calleeMethodArgs) {
        boolean skipRawMethodCall = false;

        if (JavaCGConstants.METHOD_NAME_INIT.equals(calleeMethodName)) {
            // 处理Runnable实现类
            Boolean recordedRunnable = runnableImplClassMap.get(calleeClassName);
            if (recordedRunnable != null) {
                // 不记录原始调用类型
                skipRawMethodCall = true;
                // 记录其他方法调用Runnable实现类的<init>方法
                String methodCall = String.format(format, callIdCounter.addAndGet(), CallTypeEnum.CTE_RIR.getType(), calleeClassName, calleeMethodName, calleeMethodArgs);
                MethodCallDto methodCallDto1 = MethodCallDto.genInstance(methodCall, getSourceLine());
                methodCalls.add(methodCallDto1);

                if (Boolean.FALSE.equals(recordedRunnable)) {
                    // Runnable实现类的<init>方法调用Runnable实现类的run方法
                    String runnableImplClassMethod = String.format(OTHER_METHOD_CALL_FORMAT, callIdCounter.addAndGet(), calleeClassName, calleeMethodName, calleeMethodArgs,
                            CallTypeEnum.CTE_RIR.getType(), calleeClassName, "run()");
                    MethodCallDto methodCallDto2 = MethodCallDto.genInstance(runnableImplClassMethod, JavaCGConstants.DEFAULT_LINE_NUMBER);
                    methodCalls.add(methodCallDto2);
                    // 避免<init>方法调用run()方法被添加多次
                    runnableImplClassMap.put(calleeClassName, Boolean.TRUE);
                }
            }

            // 处理Callable实现类
            Boolean recordedCallable = callableImplClassMap.get(calleeClassName);
            if (recordedCallable != null) {
                // 不记录原始调用类型
                skipRawMethodCall = true;
                // 记录其他方法调用Callable实现类的<init>方法
                String methodCall = String.format(format, callIdCounter.addAndGet(), CallTypeEnum.CTE_CIC.getType(), calleeClassName, calleeMethodName,
                        calleeMethodArgs);
                MethodCallDto methodCallDto1 = MethodCallDto.genInstance(methodCall, getSourceLine());
                methodCalls.add(methodCallDto1);

                if (Boolean.FALSE.equals(recordedCallable)) {
                    // Callable实现类的<init>方法调用Callable实现类的call方法
                    String callableImplClassMethod = String.format(OTHER_METHOD_CALL_FORMAT, callIdCounter.addAndGet(), calleeClassName, calleeMethodName, calleeMethodArgs,
                            CallTypeEnum.CTE_CIC.getType(), calleeClassName, "call()");
                    MethodCallDto methodCallDto2 = MethodCallDto.genInstance(callableImplClassMethod, JavaCGConstants.DEFAULT_LINE_NUMBER);
                    methodCalls.add(methodCallDto2);
                    // 避免<init>方法调用call()方法被添加多次
                    callableImplClassMap.put(calleeClassName, Boolean.TRUE);
                }
            }
        } else if (JavaCGConstants.METHOD_NAME_START.equals(calleeMethodName) && "()".equals(calleeMethodArgs)) {
            // 处理Thread子类
            if (Boolean.FALSE.equals(threadChildClassMap.get(calleeClassName))) {
                // 记录Thread子类的start方法调用run方法（以上Map的value等于FALSE时，代表当前类为Thread的子类，且start()方法调用run()方法未添加过）
                String threadChildClassMethod = String.format(OTHER_METHOD_CALL_FORMAT, callIdCounter.addAndGet(), calleeClassName, calleeMethodName, calleeMethodArgs,
                        CallTypeEnum.CTE_TSR.getType(), calleeClassName, "run()");
                MethodCallDto methodCallDto2 = MethodCallDto.genInstance(threadChildClassMethod, JavaCGConstants.DEFAULT_LINE_NUMBER);
                methodCalls.add(methodCallDto2);
                // 避免start()方法调用run()方法被添加多次
                threadChildClassMap.put(calleeClassName, Boolean.TRUE);
            }
        }

        return skipRawMethodCall;
    }

    // 获取源代码行号
    private int getSourceLine() {
        if (lineNumberTable == null) {
            return JavaCGConstants.DEFAULT_LINE_NUMBER;
        }
        int sourceLine = lineNumberTable.getSourceLine(ih.getPosition());
        return Math.max(sourceLine, 0);
    }
}
