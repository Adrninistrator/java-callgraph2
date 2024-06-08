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
    private final FieldInformationMap nonStaticFieldInfoMap;

    // 静态变量
    private final FieldInformationMap staticFieldInfoMap;

    public FrameSnapshotEntry(JavaCGOperandStack stackSnapshot,
                              JavaCGLocalVariables localsSnapshot,
                              FieldInformationMap nonStaticFieldInfoMap,
                              FieldInformationMap staticFieldInfoMap) {
        if (stackSnapshot == null || localsSnapshot == null || nonStaticFieldInfoMap == null || staticFieldInfoMap == null) {
            throw new JavaCGRuntimeException("传入参数为空");
        }

        this.stackSnapshot = stackSnapshot;
        this.localsSnapshot = localsSnapshot;
        this.nonStaticFieldInfoMap = nonStaticFieldInfoMap;
        this.staticFieldInfoMap = staticFieldInfoMap;
    }

    public JavaCGOperandStack getStackSnapshot() {
        return stackSnapshot;
    }

    public JavaCGLocalVariables getLocalsSnapshot() {
        return localsSnapshot;
    }

    public FieldInformationMap getNonStaticFieldInfoMap() {
        return nonStaticFieldInfoMap;
    }

    public FieldInformationMap getStaticFieldInfoMap() {
        return staticFieldInfoMap;
    }

    public JavaCGOperandStack copyStackSnapshot() {
        return stackSnapshot.copy();
    }

    public JavaCGLocalVariables copyLocalsSnapshot() {
        return localsSnapshot.copy();
    }

    public FieldInformationMap copyNonStaticFieldInfo() {
        return nonStaticFieldInfoMap.copy();
    }

    public FieldInformationMap copyStaticFieldInfo() {
        return staticFieldInfoMap.copy();
    }

    @Override
    public String toString() {
        return "stackSnapshot.size() " + stackSnapshot.size() +
                " localsSnapshot.size() " + localsSnapshot.size() +
                " nonStaticFieldInfo.size() " + nonStaticFieldInfoMap.size() +
                " staticFieldInfo.size() " + staticFieldInfoMap.size();
    }
}
