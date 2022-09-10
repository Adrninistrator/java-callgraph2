package com.adrninistrator.javacg.visitor;

import com.adrninistrator.javacg.common.ClassNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.counter.CallIdCounter;
import com.adrninistrator.javacg.dto.method.MethodCallDto;
import com.adrninistrator.javacg.dto.method.MethodLineNumberInfo;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatorInterface;
import com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesHandler;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.MethodGen;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

// 处理Class对象
public class ClassVisitor {

    private JavaClass javaClass;

    private ConstantPoolGen cpg;

    private String classReferenceFormat;

    private List<MethodCallDto> methodCallList = new ArrayList<>(200);

    private List<MethodLineNumberInfo> methodLineNumberList = new ArrayList<>(100);

    private Map<String, Set<String>> calleeMethodMapGlobal;

    private Map<String, Boolean> runnableImplClassMap;

    private Map<String, Boolean> callableImplClassMap;

    private Map<String, Boolean> threadChildClassMap;

    private CallIdCounter callIdCounter;

    private List<CustomCodeParserInterface> customCodeParserList;

    private boolean recordAll = false;

    private Writer annotationWriter;

    private AnnotationAttributesFormatorInterface annotationAttributesFormator;

    public ClassVisitor(JavaClass javaClass) {
        this.javaClass = javaClass;
        cpg = new ConstantPoolGen(javaClass.getConstantPool());
        classReferenceFormat = JavaCGConstants.FILE_KEY_CLASS_PREFIX + javaClass.getClassName() + " %s";
    }

    public void visitConstantPool() {
        ConstantPool constantPool = javaClass.getConstantPool();

        Set<String> referencedClassSet = new HashSet<>();

        for (int i = 0; i < constantPool.getLength(); i++) {
            Constant constant = constantPool.getConstant(i);
            if (constant == null || constant.getTag() != Const.CONSTANT_Class) {
                continue;
            }

            String referencedClass = constantPool.constantToString(constant);
            // 对Object类的引用不处理
            if (!ClassNameConstants.CLASS_NAME_OBJECT.equals(referencedClass)) {
                referencedClass = JavaCGUtil.handleClassNameWithArray(referencedClass);

                referencedClassSet.add(referencedClass);
            }
        }

        // 将当前类也添加到当前类调用的类列表中，防止没有被其他类调用的类名不被记录（当前类应该会在当前类的CONSTANT_Class常量中，但为了保险起见，再添加一次）
        referencedClassSet.add(javaClass.getClassName());

        List<String> referencedClassList = new ArrayList<>(referencedClassSet);
        Collections.sort(referencedClassList);

        for (String referencedClass : referencedClassList) {
            MethodCallDto methodCallDto = MethodCallDto.genInstance4Class(String.format(classReferenceFormat, referencedClass),
                    JavaCGConstants.NONE_LINE_NUMBER);
            methodCallList.add(methodCallDto);
        }
    }

    public void visitMethod(Method method) {
        try {
            MethodGen mg = new MethodGen(method, javaClass.getClassName(), cpg);
            MethodVisitor visitor = new MethodVisitor(mg, javaClass);
            visitor.setMethodCallList(methodCallList);
            visitor.setMethodLineNumberList(methodLineNumberList);
            visitor.setCalleeMethodMapGlobal(calleeMethodMapGlobal);
            visitor.setRunnableImplClassMap(runnableImplClassMap);
            visitor.setCallableImplClassMap(callableImplClassMap);
            visitor.setThreadChildClassMap(threadChildClassMap);
            visitor.setCallIdCounter(callIdCounter);
            visitor.setCustomCodeParserList(customCodeParserList);
            visitor.setRecordAll(recordAll);
            visitor.setAnnotationWriter(annotationWriter);
            visitor.setAnnotationAttributesFormator(annotationAttributesFormator);

            visitor.beforeStart();
            visitor.start();
        } catch (Exception e) {
            System.err.println("处理方法出现异常 " + javaClass.getClassName() + " " + method.getName());
            e.printStackTrace();
        }
    }

    public void start() {
        visitConstantPool();

        if (!javaClass.isAnnotation()) {
            // 记录类上的注解信息，不处理注解类型的类
            AnnotationAttributesHandler.writeAnnotationInfo(JavaCGConstants.FILE_KEY_CLASS_PREFIX,
                    javaClass.getClassName(),
                    javaClass.getAnnotationEntries(),
                    annotationAttributesFormator,
                    annotationWriter);
        }

        for (Method method : javaClass.getMethods()) {
            visitMethod(method);
        }
    }

    public List<MethodCallDto> getMethodCallList() {
        return methodCallList;
    }

    public List<MethodLineNumberInfo> getMethodLineNumberList() {
        return methodLineNumberList;
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

    public void setCallIdCounter(CallIdCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setCustomCodeParserList(List<CustomCodeParserInterface> customCodeParserList) {
        this.customCodeParserList = customCodeParserList;
    }

    public void setRecordAll(boolean recordAll) {
        this.recordAll = recordAll;
    }

    public void setAnnotationWriter(Writer annotationWriter) {
        this.annotationWriter = annotationWriter;
    }

    public void setAnnotationAttributesFormator(AnnotationAttributesFormatorInterface annotationAttributesFormator) {
        this.annotationAttributesFormator = annotationAttributesFormator;
    }
}
