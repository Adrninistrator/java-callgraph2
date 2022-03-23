package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.CallIdCounter;
import com.adrninistrator.javacg.dto.MethodCallDto;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;

import java.util.*;

// 处理Class对象
public class ClassVisitor {

    private JavaClass clazz;
    private ConstantPoolGen constants;
    private String classReferenceFormat;
    private List<MethodCallDto> methodCalls = new ArrayList<>();

    private Map<String, Set<String>> calleeMethodMapGlobal;
    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> threadChildClassMap;
    private Map<String, Set<String>> methodAnnotationMap;
    private CallIdCounter callIdCounter;
    private List<CustomCodeParserInterface> customCodeParserList;

    private boolean recordAll = false;

    public ClassVisitor(JavaClass jc) {
        clazz = jc;
        constants = new ConstantPoolGen(clazz.getConstantPool());
        classReferenceFormat = JavaCGConstants.FILE_KEY_CLASS_PREFIX + clazz.getClassName() + " %s";
    }

    public void visitConstantPool() {
        ConstantPool constantPool = clazz.getConstantPool();

        Set<String> referencedClassSet = new HashSet<>();

        for (int i = 0; i < constantPool.getLength(); i++) {
            Constant constant = constantPool.getConstant(i);
            if (constant == null || constant.getTag() != Const.CONSTANT_Class) {
                continue;
            }

            String referencedClass = constantPool.constantToString(constant);
            // 对当前类自身的引用不处理
            if (!clazz.getClassName().equals(referencedClass) && !JavaCGConstants.OBJECT_CLASS_NAME.equals(referencedClass)) {
                referencedClassSet.add(referencedClass);
            }
        }

        List<String> referencedClassList = new ArrayList<>(referencedClassSet);
        Collections.sort(referencedClassList);

        for (String referencedClass : referencedClassList) {
            MethodCallDto methodCallDto = MethodCallDto.genInstance(String.format(classReferenceFormat, referencedClass),
                    JavaCGConstants.NONE_LINE_NUMBER);
            methodCalls.add(methodCallDto);
        }
    }

    public void visitMethod(Method method) {
        MethodGen mg = new MethodGen(method, clazz.getClassName(), constants);
        MethodVisitor visitor = new MethodVisitor(mg, clazz);
        visitor.setCalleeMethodMapGlobal(calleeMethodMapGlobal);
        visitor.setRunnableImplClassMap(runnableImplClassMap);
        visitor.setCallableImplClassMap(callableImplClassMap);
        visitor.setThreadChildClassMap(threadChildClassMap);
        visitor.setMethodAnnotationMap(methodAnnotationMap);
        visitor.setCallIdCounter(callIdCounter);
        visitor.setCustomCodeParserList(customCodeParserList);
        visitor.setRecordAll(recordAll);

        visitor.beforeStart();
        List<MethodCallDto> methodCallDtos = visitor.start();
        methodCalls.addAll(methodCallDtos);
    }

    public void start() {
        visitConstantPool();

        for (Method method : clazz.getMethods()) {
            visitMethod(method);
        }
    }

    public List<MethodCallDto> methodCalls() {
        return this.methodCalls;
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
}
