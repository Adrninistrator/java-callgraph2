package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumberTable;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;

import java.io.IOException;

/**
 * @author adrninistrator
 * @date 2022/11/2
 * @description: 对方法进行处理，基类
 */
public abstract class AbstractMethodHandler {

    protected JavaClass javaClass;

    protected MethodGen mg;

    protected ConstantPoolGen cpg;

    protected LineNumberTable lineNumberTable;

    protected InstructionHandle ih;

    // 需要解析方法调用的可能的类型与值的开关
    protected boolean parseMethodCallTypeValueFlag;

    protected JavaCGConfInfo javaCGConfInfo;

    protected AbstractMethodHandler(MethodGen mg, JavaClass javaClass, JavaCGConfInfo javaCGConfInfo) {
        this.javaClass = javaClass;
        this.mg = mg;
        this.javaCGConfInfo = javaCGConfInfo;
        cpg = mg.getConstantPool();
        lineNumberTable = mg.getLineNumberTable(cpg);
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
//        if (!"xxx".equals(mg.getClassName())
//                || !"xxx".equals(mg.getName())
////                || mg.getArgumentTypes().length != 2
////                || !"xxx".equals(mg.getArgumentType(0).toString())
//        ) {
//            System.out.println("跳过当前方法 " + mg.getName());
//            return true;
//        }

        try {
            /*
                首先进行方法预处理
                若方法需要处理则进行处理
                若处理失败则返回
             */
            if (preHandleMethod() &&
                    !doHandleMethod()) {
                return false;
            }

            /*
                执行最后阶段的处理
                若方法不需要处理，或需要处理且处理正常，都需要执行最后阶段的处理
             */
            return lastStep();
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("处理方法出现异常，需要分析原因 " + javaClass.getClassName() + " " + mg.getName());

            if (javaCGConfInfo != null && !javaCGConfInfo.isContinueWhenError()) {
                System.out.println("假如在处理方法出现异常时需要继续执行，请在" + JavaCGConstants.FILE_CONFIG + "参数中指定" +
                        JavaCGConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR.getKey() + JavaCGConstants.FLAG_EQUAL + Boolean.TRUE);
                return false;
            }
            return true;
        }
    }

    // 获取源代码行号
    protected int getSourceLine() {
        if (lineNumberTable == null) {
            return JavaCGConstants.DEFAULT_LINE_NUMBER;
        }
        int sourceLine = lineNumberTable.getSourceLine(ih.getPosition());
        return Math.max(sourceLine, 0);
    }

    // 设置需要记录方法调用的可能的类型与值的开关
    public void setParseMethodCallTypeValueFlag(boolean parseMethodCallTypeValueFlag) {
        this.parseMethodCallTypeValueFlag = parseMethodCallTypeValueFlag;
    }
}
