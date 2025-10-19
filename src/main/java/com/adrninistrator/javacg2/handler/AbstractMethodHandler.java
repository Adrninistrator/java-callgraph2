package com.adrninistrator.javacg2.handler;

import com.adrninistrator.javacg2.conf.JavaCG2ConfInfo;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.field.FieldPossibleTypes;
import com.adrninistrator.javacg2.dto.fieldrelationship.GetSetFieldRelationship;
import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.dto.instruction.InvokeInstructionPosAndCallee;
import com.adrninistrator.javacg2.dto.type.JavaCG2GenericsType;
import com.adrninistrator.javacg2.dto.type.JavaCG2Type;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumberTable;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/2
 * @description: 对方法进行处理，基类
 */
public abstract class AbstractMethodHandler {

    private static final Logger logger = LoggerFactory.getLogger(AbstractMethodHandler.class);

    protected final String callerClassName;

    protected final String callerMethodName;

    protected final Type[] callerArgTypes;

    protected final String callerFullMethod;

    protected final Method method;

    protected final MethodGen mg;

    protected final JavaClass javaClass;

    protected final JavaCG2InputAndOutput javaCG2InputAndOutput;

    protected final JavaCG2ConfInfo javaCG2ConfInfo;

    protected final ConstantPoolGen cpg;

    protected final LineNumberTable lineNumberTable;

    protected final Boolean continueWhenError;

    protected final JavaCG2ElManager javaCG2ElManager;

    protected InstructionHandle ih;

    // 需要解析方法调用的可能的类型与值的开关
    protected boolean parseMethodCallTypeValueFlag;

    // 当前方法是否为静态代码块
    protected boolean inClinitMethod;

    // 当前类已记录过的set方法名（get方法没有参数，只会有一个）
    protected Set<String> recordedSetMethodSet;

    protected JavaCG2Counter failCounter;

    protected Writer enumInitArgFieldWriter;
    protected Writer enumInitAssignInfoWriter;
    protected Writer getMethodWriter;
    protected Writer setMethodWriter;
    protected Writer methodReturnConstValueWriter;
    protected Writer methodReturnFieldInfoWriter;

    // 非静态字段字段所有可能的类型
    protected FieldPossibleTypes nonStaticFieldPossibleTypes;

    /*
        当前类中的字段名称与类型Map
        key     字段名称
        value   字段类型
     */
    protected Map<String, JavaCG2Type> nonStaticFieldNameTypeMap;

    /*
        记录当前类非静态的字段名称及集合类型中的泛型类型Map
        key     字段名称
        value   集合类型中的泛型类型
     */
    protected Map<String, List<JavaCG2GenericsType>> nonStaticFieldNameGenericsTypeMap;

    /*
        记录当前类的static、final字段名称及初始化方法指令位置的Map
        key     当前类的static、final字段名称
        value   初始化方法指令位置列表
     */
    protected Map<String, List<InvokeInstructionPosAndCallee>> sfFieldInvokeInstructionMap;

    // 保存作为当前方法返回的方法参数序号
    protected List<Integer> methodReturnArgSeqList;

    // 保存作为当前方法返回的方法调用指令位置
    protected List<Integer> methodReturnPositionList;

    // 保存等值转换前作为当前方法返回的方法参数序号
    protected List<Integer> methodReturnArgSeqEQCList;

    // 保存等值转换前作为当前方法返回的方法调用指令位置
    protected List<Integer> methodReturnPositionEQCList;

    // 记录通过get/set方法关联的字段关系信息
    protected List<GetSetFieldRelationship> getSetFieldRelationshipList;

    // 存在get方法的字段名称Set
    protected Set<String> fieldWithGetMethodNameSet = new HashSet<>();

    // 存在set方法的字段名称Set
    protected Set<String> fieldWithSetMethodNameSet = new HashSet<>();

    protected JavaCG2Counter fieldRelationshipCounter;

    // 当前方法的返回类型
    protected String methodReturnType;

    protected AbstractMethodHandler(Method method, MethodGen mg, JavaClass javaClass, String callerFullMethod, JavaCG2InputAndOutput javaCG2InputAndOutput) {
        this.method = method;
        this.mg = mg;
        this.javaClass = javaClass;
        this.callerFullMethod = callerFullMethod;
        this.javaCG2InputAndOutput = javaCG2InputAndOutput;

        javaCG2ConfInfo = javaCG2InputAndOutput.getJavaCG2ConfInfo();
        callerClassName = javaClass.getClassName();
        callerMethodName = mg.getName();
        callerArgTypes = mg.getArgumentTypes();
        cpg = mg.getConstantPool();
        lineNumberTable = mg.getLineNumberTable(cpg);
        methodReturnType = mg.getReturnType().toString();
        continueWhenError = javaCG2InputAndOutput.getJavaCG2ConfigureWrapper().getMainConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR);
        javaCG2ElManager = javaCG2InputAndOutput.getJavaCG2ElManager();
    }

    /**
     * 初始化
     */
    protected void init() {
    }

    /**
     * 方法预处理
     *
     * @return false: 方法不需要继续处理 true: 方法需要继续处理
     */
    protected abstract boolean preHandleMethod() throws IOException;

    /**
     * 执行处理方法
     *
     * @return false: 处理失败 true: 处理成功
     */
    protected abstract boolean doHandleMethod() throws IOException;

    /**
     * 最后阶段的处理
     *
     * @return false: 处理失败 true: 处理成功
     */
    protected boolean lastStep() throws IOException {
        return true;
    }

    /**
     * 处理方法
     *
     * @return false: 处理失败 true: 处理成功
     */
    public boolean handleMethod() {
//        if (!"xxx".equals(callerClassName)
//                || !"xxx".equals(callerMethodName)
////                || callerArgTypes.length != 2
////                || !"xxx".equals(mg.getArgumentType(0).toString())
//        ) {
//            logger.info("跳过当前方法 {}:{}", callerClassName, callerMethodName);
//            return true;
//        }

        try {
            // 初始化
            init();

            /*
                先进行方法预处理
                若方法需要处理则进行处理
                若处理失败则返回
             */
            if (preHandleMethod() &&
                    !doHandleMethod()) {
                // 增加失败次数
                failCounter.addAndGet();
                return continueWhenError;
            }

            /*
                执行最后阶段的处理
                若方法不需要处理，或需要处理且处理正常，都需要执行最后阶段的处理
             */
            boolean success = lastStep();
            if (!success) {
                // 增加失败次数
                failCounter.addAndGet();
            }
            return success;
        } catch (Exception e) {
            logger.error("处理方法出现异常，需要分析原因 {} {} ", callerFullMethod, methodReturnType, e);
            // 增加失败次数
            failCounter.addAndGet();

            if (Boolean.TRUE.equals(continueWhenError)) {
                return true;
            }
            logger.info("假如在处理方法出现异常时需要继续执行，请配置参数值为 {} {}", Boolean.TRUE,
                    javaCG2InputAndOutput.getJavaCG2ConfigureWrapper().genConfigUsage(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR)
            );
            return false;
        }
    }

    // 获取源代码行号
    protected int getSourceLine() {
        return JavaCG2ByteCodeUtil.getSourceLine(ih.getPosition(), lineNumberTable);
    }

    // 获取源代码行号
    protected int getSourceLine(int instructionPosition) {
        return JavaCG2ByteCodeUtil.getSourceLine(instructionPosition, lineNumberTable);
    }

    // 设置需要记录方法调用的可能的类型与值的开关
    public void setParseMethodCallTypeValueFlag(boolean parseMethodCallTypeValueFlag) {
        this.parseMethodCallTypeValueFlag = parseMethodCallTypeValueFlag;
    }

    public void setInClinitMethod(boolean inClinitMethod) {
        this.inClinitMethod = inClinitMethod;
    }

    public void setRecordedSetMethodSet(Set<String> recordedSetMethodSet) {
        this.recordedSetMethodSet = recordedSetMethodSet;
    }

    public void setFailCounter(JavaCG2Counter failCounter) {
        this.failCounter = failCounter;
    }

    public void setEnumInitArgFieldWriter(Writer enumInitArgFieldWriter) {
        this.enumInitArgFieldWriter = enumInitArgFieldWriter;
    }

    public void setEnumInitAssignInfoWriter(Writer enumInitAssignInfoWriter) {
        this.enumInitAssignInfoWriter = enumInitAssignInfoWriter;
    }

    public void setGetMethodWriter(Writer getMethodWriter) {
        this.getMethodWriter = getMethodWriter;
    }

    public void setSetMethodWriter(Writer setMethodWriter) {
        this.setMethodWriter = setMethodWriter;
    }

    public void setMethodReturnConstValueWriter(Writer methodReturnConstValueWriter) {
        this.methodReturnConstValueWriter = methodReturnConstValueWriter;
    }

    public void setMethodReturnFieldInfoWriter(Writer methodReturnFieldInfoWriter) {
        this.methodReturnFieldInfoWriter = methodReturnFieldInfoWriter;
    }

    public void setNonStaticFieldPossibleTypes(FieldPossibleTypes nonStaticFieldPossibleTypes) {
        this.nonStaticFieldPossibleTypes = nonStaticFieldPossibleTypes;
    }

    public void setNonStaticFieldNameTypeMap(Map<String, JavaCG2Type> nonStaticFieldNameTypeMap) {
        this.nonStaticFieldNameTypeMap = nonStaticFieldNameTypeMap;
    }

    public void setNonStaticFieldNameGenericsTypeMap(Map<String, List<JavaCG2GenericsType>> nonStaticFieldNameGenericsTypeMap) {
        this.nonStaticFieldNameGenericsTypeMap = nonStaticFieldNameGenericsTypeMap;
    }

    public void setSfFieldInvokeInstructionMap(Map<String, List<InvokeInstructionPosAndCallee>> sfFieldInvokeInstructionMap) {
        this.sfFieldInvokeInstructionMap = sfFieldInvokeInstructionMap;
    }

    public void setMethodReturnArgSeqList(List<Integer> methodReturnArgSeqList) {
        this.methodReturnArgSeqList = methodReturnArgSeqList;
    }

    public void setMethodReturnPositionList(List<Integer> methodReturnPositionList) {
        this.methodReturnPositionList = methodReturnPositionList;
    }

    public void setMethodReturnArgSeqEQCList(List<Integer> methodReturnArgSeqEQCList) {
        this.methodReturnArgSeqEQCList = methodReturnArgSeqEQCList;
    }

    public void setMethodReturnPositionEQCList(List<Integer> methodReturnPositionEQCList) {
        this.methodReturnPositionEQCList = methodReturnPositionEQCList;
    }

    public void setGetSetFieldRelationshipList(List<GetSetFieldRelationship> getSetFieldRelationshipList) {
        this.getSetFieldRelationshipList = getSetFieldRelationshipList;
    }

    public void setFieldRelationshipCounter(JavaCG2Counter fieldRelationshipCounter) {
        this.fieldRelationshipCounter = fieldRelationshipCounter;
    }

    public void setFieldWithGetMethodNameSet(Set<String> fieldWithGetMethodNameSet) {
        this.fieldWithGetMethodNameSet = fieldWithGetMethodNameSet;
    }

    public void setFieldWithSetMethodNameSet(Set<String> fieldWithSetMethodNameSet) {
        this.fieldWithSetMethodNameSet = fieldWithSetMethodNameSet;
    }
}
