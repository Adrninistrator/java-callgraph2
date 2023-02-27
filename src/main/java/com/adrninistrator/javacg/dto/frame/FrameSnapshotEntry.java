package com.adrninistrator.javacg.dto.frame;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;

/**
 * @author adrninistrator
 * @date 2022/10/21
 * @description: 栈桢对应的信息快照
 */
public class FrameSnapshotEntry {

    // 操作数栈
    private final JavaCGOperandStack stackSnapshot;

    // 本地变量
    private final JavaCGLocalVariables localsSnapshot;

    // 非静态变量
    private final FieldInformation nonStaticFieldInfo;

    // 静态变量
    private final FieldInformation staticFieldInfo;

    public FrameSnapshotEntry(JavaCGOperandStack stackSnapshot,
                              JavaCGLocalVariables localsSnapshot,
                              FieldInformation nonStaticFieldInfo,
                              FieldInformation staticFieldInfo) {
        if (stackSnapshot == null || localsSnapshot == null || nonStaticFieldInfo == null || staticFieldInfo == null) {
            throw new JavaCGRuntimeException("传入参数为空");
        }

        this.stackSnapshot = stackSnapshot;
        this.localsSnapshot = localsSnapshot;
        this.nonStaticFieldInfo = nonStaticFieldInfo;
        this.staticFieldInfo = staticFieldInfo;
    }

    public JavaCGOperandStack getStackSnapshot() {
        return stackSnapshot;
    }

    public JavaCGLocalVariables getLocalsSnapshot() {
        return localsSnapshot;
    }

    public FieldInformation getNonStaticFieldInfo() {
        return nonStaticFieldInfo;
    }

    public FieldInformation getStaticFieldInfo() {
        return staticFieldInfo;
    }

    public JavaCGOperandStack copyStackSnapshot() {
        return stackSnapshot.copy();
    }

    public JavaCGLocalVariables copyLocalsSnapshot() {
        return localsSnapshot.copy();
    }

    public FieldInformation copyNonStaticFieldInfo() {
        return nonStaticFieldInfo.copy();
    }

    public FieldInformation copyStaticFieldInfo() {
        return staticFieldInfo.copy();
    }

    @Override
    public String toString() {
        return "stackSnapshot.size() " + stackSnapshot.size() +
                " localsSnapshot.size() " + localsSnapshot.size() +
                " nonStaticFieldInfo.size() " + nonStaticFieldInfo.size() +
                " staticFieldInfo.size() " + staticFieldInfo.size();
    }
}
