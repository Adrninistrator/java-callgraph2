package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.Constants;
import com.adrninistrator.javacg.dto.CallIdCounter;
import com.adrninistrator.javacg.dto.MethodCallDto;
import com.adrninistrator.javacg.dto.MethodInfo;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.extension.interfaces.CustomHandlerInterface;
import com.adrninistrator.javacg.util.CommonUtil;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.EmptyVisitor;
import org.apache.bcel.generic.*;

import java.util.*;

// 处理Method对象
public class MethodVisitor extends EmptyVisitor {

    private JavaClass visitedClass;
    private MethodGen mg;
    private ConstantPoolGen cp;
    private String format;
    private List<MethodCallDto> methodCalls = new ArrayList<>();
    private LineNumberTable lineNumberTable;
    private InstructionHandle ih;
    private Map<String, Set<String>> calleeMethodMap;
    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> threadChildClassMap;
    private Map<String, Set<String>> methodAnnotationMap;
    private CallIdCounter callIdCounter;
    private List<CustomHandlerInterface> customHandlerList;

    public MethodVisitor(MethodGen m, JavaClass jc) {
        visitedClass = jc;
        mg = m;
        cp = mg.getConstantPool();

        lineNumberTable = mg.getLineNumberTable(cp);
    }

    public void setCalleeMethodMap(Map<String, Set<String>> calleeMethodMap) {
        this.calleeMethodMap = calleeMethodMap;
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

    public void setCustomInterfaceList(List<CustomHandlerInterface> customHandlerList) {
        this.customHandlerList = customHandlerList;
    }

    public void beforeStart() {
        String fullMethod = visitedClass.getClassName() + ":" + mg.getName() + CommonUtil.argumentList(mg.getArgumentTypes());

        handleAnnotationName(fullMethod);

        format = "M:%d " + fullMethod + " " + "(%s)%s:%s%s";
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

            if (!visitInstruction(i)) {
                i.accept(this);
            }
        }
        return methodCalls;
    }

    private boolean visitInstruction(Instruction i) {
        short opcode = i.getOpcode();
        return ((InstructionConst.getInstruction(opcode) != null)
                && !(i instanceof ConstantPushInstruction)
                && !(i instanceof ReturnInstruction));
    }

    @Override
    public void visitINVOKEVIRTUAL(INVOKEVIRTUAL i) {
        addMethodCalls("M", i.getReferenceType(cp).toString(), i.getMethodName(cp), i.getArgumentTypes(cp));
    }

    @Override
    public void visitINVOKEINTERFACE(INVOKEINTERFACE i) {
        addMethodCalls("I", i.getReferenceType(cp).toString(), i.getMethodName(cp), i.getArgumentTypes(cp));
    }

    @Override
    public void visitINVOKESPECIAL(INVOKESPECIAL i) {
        addMethodCalls("O", i.getReferenceType(cp).toString(), i.getMethodName(cp), i.getArgumentTypes(cp));
    }

    @Override
    public void visitINVOKESTATIC(INVOKESTATIC i) {
        addMethodCalls("S", i.getReferenceType(cp).toString(), i.getMethodName(cp), i.getArgumentTypes(cp));
    }

    @Override
    public void visitINVOKEDYNAMIC(INVOKEDYNAMIC i) {
        addMethodCalls("D", i.getType(cp).toString(), i.getMethodName(cp), i.getArgumentTypes(cp));

        Constant constantID = cp.getConstant(i.getIndex());
        if (!(constantID instanceof ConstantInvokeDynamic)) {
            return;
        }

        // 处理Lambda表达式
        ConstantInvokeDynamic cid = (ConstantInvokeDynamic) constantID;
        // 获得JavaClass中指定下标的BootstrapMethod
        BootstrapMethod bootstrapMethod = CommonUtil.getBootstrapMethod(visitedClass, cid.getBootstrapMethodAttrIndex());
        if (bootstrapMethod == null) {
            System.err.println("### 无法找到bootstrapMethod " + cid.getBootstrapMethodAttrIndex());
            return;
        }

        // 获得BootstrapMethod的方法信息
        MethodInfo bootstrapMethodMethod = CommonUtil.getBootstrapMethodMethod(bootstrapMethod, visitedClass);
        if (bootstrapMethodMethod == null) {
            System.err.println("### 无法找到bootstrapMethod的方法信息 " + visitedClass.getClassName() + " " + bootstrapMethod);
            return;
        }

        String callType = bootstrapMethodMethod.getMethodName().startsWith(Constants.FLAG_LAMBDA) ? CallTypeEnum.CTE_LM.getType() : CallTypeEnum.CTE_ST.getType();
        addMethodCalls(callType, bootstrapMethodMethod.getClassName(), bootstrapMethodMethod.getMethodName(), bootstrapMethodMethod.getMethodArgumentTypes());
    }

    private void addMethodCalls(String type, String calleeClassName, String calleeMethodName, Type[] arguments) {
        // 添加被调用方法信息
        String calleeMethodArgs = CommonUtil.argumentList(arguments);

        Set<String> calleeMethodWithArgsSet = calleeMethodMap.get(calleeClassName);
        if (calleeMethodWithArgsSet == null) {
            calleeMethodWithArgsSet = new HashSet<>();
            calleeMethodMap.put(calleeClassName, calleeMethodWithArgsSet);
        }
        calleeMethodWithArgsSet.add(calleeMethodName + calleeMethodArgs);

        boolean skipRawMethodCall = false;

        if (Constants.METHOD_NAME_INIT.equals(calleeMethodName)) {
            // 处理Runnable实现类
            Boolean recordedRunnable = runnableImplClassMap.get(calleeClassName);
            if (recordedRunnable != null) {
                // 不记录原始调用类型
                skipRawMethodCall = true;
                // 其他方法调用Runnable实现类的<init>方法
                String methodCall = String.format(format, callIdCounter.addAndGet(), CallTypeEnum.CTE_RIR.getType(), calleeClassName, calleeMethodName, calleeMethodArgs);
                MethodCallDto methodCallDto1 = MethodCallDto.genInstance(methodCall, getSourceLine());
                methodCalls.add(methodCallDto1);

                if (Boolean.FALSE.equals(recordedRunnable)) {
                    // Runnable实现类的<init>方法调用Runnable实现类的run方法
                    String runnableImplClassMethod = String.format("M:%d %s:%s%s (%s)%s:run()", callIdCounter.addAndGet(), calleeClassName, calleeMethodName, calleeMethodArgs,
                            CallTypeEnum.CTE_RIR.getType(), calleeClassName);
                    MethodCallDto methodCallDto2 = MethodCallDto.genInstance(runnableImplClassMethod, Constants.DEFAULT_LINE_NUMBER);
                    methodCalls.add(methodCallDto2);

                    runnableImplClassMap.put(calleeClassName, Boolean.TRUE);
                }
            }

            // 处理Callable实现类
            Boolean recordedCallable = callableImplClassMap.get(calleeClassName);
            if (recordedCallable != null) {
                // 不记录原始调用类型
                skipRawMethodCall = true;
                // 其他方法调用Callable实现类的<init>方法
                String methodCall = String.format(format, callIdCounter.addAndGet(), CallTypeEnum.CTE_CIC.getType(), calleeClassName, calleeMethodName,
                        calleeMethodArgs);
                MethodCallDto methodCallDto1 = MethodCallDto.genInstance(methodCall, getSourceLine());
                methodCalls.add(methodCallDto1);

                if (Boolean.FALSE.equals(recordedCallable)) {
                    // Callable实现类的<init>方法调用Callable实现类的call方法
                    String callableImplClassMethod = String.format("M:%d %s:%s%s (%s)%s:call()", callIdCounter.addAndGet(), calleeClassName, calleeMethodName, calleeMethodArgs,
                            CallTypeEnum.CTE_CIC.getType(), calleeClassName);
                    MethodCallDto methodCallDto2 = MethodCallDto.genInstance(callableImplClassMethod, Constants.DEFAULT_LINE_NUMBER);
                    methodCalls.add(methodCallDto2);

                    callableImplClassMap.put(calleeClassName, Boolean.TRUE);
                }
            }
        } else if (Constants.METHOD_NAME_START.equals(calleeMethodName) && "()".equals(calleeMethodArgs)) {
            // 处理Thread子类
            if (Boolean.FALSE.equals(threadChildClassMap.get(calleeClassName))) {
                // Thread子类的start方法调用run方法
                String threadChildClassMethod = String.format("M:%d %s:%s%s (%s)%s:run()", callIdCounter.addAndGet(), calleeClassName, calleeMethodName, calleeMethodArgs,
                        CallTypeEnum.CTE_TSR.getType(), calleeClassName);
                MethodCallDto methodCallDto2 = MethodCallDto.genInstance(threadChildClassMethod, Constants.DEFAULT_LINE_NUMBER);
                methodCalls.add(methodCallDto2);

                threadChildClassMap.put(calleeClassName, Boolean.TRUE);
            }
        }

        if (skipRawMethodCall) {
            return;
        }

        int callId = callIdCounter.addAndGet();
        String methodCall = String.format(format, callId, type, calleeClassName, calleeMethodName, calleeMethodArgs);
        MethodCallDto methodCallDto = MethodCallDto.genInstance(methodCall, getSourceLine());
        methodCalls.add(methodCallDto);

        // 调用自定义接口实现类的方法
        for (CustomHandlerInterface customHandler : customHandlerList) {
            customHandler.handleMethodCall(callId, calleeClassName, calleeMethodName, arguments, ih, mg);
        }
    }

    // 获取源代码行号
    private int getSourceLine() {
        if (lineNumberTable == null) {
            return Constants.DEFAULT_LINE_NUMBER;
        }
        int sourceLine = lineNumberTable.getSourceLine(ih.getPosition());
        return Math.max(sourceLine, 0);
    }
}
