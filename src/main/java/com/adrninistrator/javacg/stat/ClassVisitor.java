package com.adrninistrator.javacg.stat;

import com.adrninistrator.javacg.common.Constants;
import com.adrninistrator.javacg.dto.CallIdCounter;
import com.adrninistrator.javacg.dto.MethodCallDto;
import com.adrninistrator.javacg.extension.interfaces.CustomHandlerInterface;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

// 处理Class对象
public class ClassVisitor extends EmptyVisitor {

    private JavaClass clazz;
    private ConstantPoolGen constants;
    private String classReferenceFormat;
    private List<MethodCallDto> methodCalls = new ArrayList<>();

    private Map<String, Set<String>> calleeMethodMap;
    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> threadChildClassMap;
    private Map<String, Set<String>> methodAnnotationMap;
    private CallIdCounter callIdCounter;
    private List<CustomHandlerInterface> customHandlerList;

    public ClassVisitor(JavaClass jc) {
        clazz = jc;
        constants = new ConstantPoolGen(clazz.getConstantPool());
        classReferenceFormat = "C:" + clazz.getClassName() + " %s";
    }

    @Override
    public void visitJavaClass(JavaClass jc) {
        jc.getConstantPool().accept(this);
        for (Method method : jc.getMethods()) {
            method.accept(this);
        }
    }

    @Override
    public void visitConstantPool(ConstantPool constantPool) {
        for (int i = 0; i < constantPool.getLength(); i++) {
            Constant constant = constantPool.getConstant(i);
            if (constant == null) {
                continue;
            }
            if (constant.getTag() == Const.CONSTANT_Class) {
                String referencedClass = constantPool.constantToString(constant);

                MethodCallDto methodCallDto = MethodCallDto.genInstance(String.format(classReferenceFormat, referencedClass),
                        Constants.NONE_LINE_NUMBER);
                methodCalls.add(methodCallDto);
            }
        }
    }

    @Override
    public void visitMethod(Method method) {
        MethodGen mg = new MethodGen(method, clazz.getClassName(), constants);
        MethodVisitor visitor = new MethodVisitor(mg, clazz);
        visitor.setCalleeMethodMap(calleeMethodMap);
        visitor.setRunnableImplClassMap(runnableImplClassMap);
        visitor.setCallableImplClassMap(callableImplClassMap);
        visitor.setThreadChildClassMap(threadChildClassMap);
        visitor.setMethodAnnotationMap(methodAnnotationMap);
        visitor.setCallIdCounter(callIdCounter);
        visitor.setCustomInterfaceList(customHandlerList);
        visitor.beforeStart();
        List<MethodCallDto> methodCallDtos = visitor.start();
        methodCalls.addAll(methodCallDtos);
    }

    public ClassVisitor start() {
        visitJavaClass(clazz);
        return this;
    }

    public List<MethodCallDto> methodCalls() {
        return this.methodCalls;
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
}
