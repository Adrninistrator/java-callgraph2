package com.adrninistrator.javacg2.dto.frame;

import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2022/10/21
 * @description: 栈桢对应的信息快照
 */
public class FrameSnapshotEntry {

    // 操作数栈
    private final JavaCG2OperandStack stackSnapshot;

    // 本地变量
    private final JavaCG2LocalVariables localsSnapshot;

    // 非静态变量
    private final FieldInformationMap nonStaticFieldInfoMap;

    // 静态变量
    private final FieldInformationMap staticFieldInfoMap;

    public FrameSnapshotEntry(JavaCG2OperandStack stackSnapshot,
                              JavaCG2LocalVariables localsSnapshot,
                              FieldInformationMap nonStaticFieldInfoMap,
                              FieldInformationMap staticFieldInfoMap) {
        if (stackSnapshot == null || localsSnapshot == null || nonStaticFieldInfoMap == null || staticFieldInfoMap == null) {
            throw new JavaCG2RuntimeException("传入参数为空");
        }

        this.stackSnapshot = stackSnapshot;
        this.localsSnapshot = localsSnapshot;
        this.nonStaticFieldInfoMap = nonStaticFieldInfoMap;
        this.staticFieldInfoMap = staticFieldInfoMap;
    }

    public JavaCG2OperandStack getStackSnapshot() {
        return stackSnapshot;
    }

    public JavaCG2LocalVariables getLocalsSnapshot() {
        return localsSnapshot;
    }

    public FieldInformationMap getNonStaticFieldInfoMap() {
        return nonStaticFieldInfoMap;
    }

    public FieldInformationMap getStaticFieldInfoMap() {
        return staticFieldInfoMap;
    }

    public JavaCG2OperandStack copyStackSnapshot() {
        return stackSnapshot.copy();
    }

    public JavaCG2LocalVariables copyLocalsSnapshot() {
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
        return "localsHash " + localsSnapshot.genHash() +
                " stackSnapshot.size() " + stackSnapshot.size() +
                " localsSnapshot.size() " + localsSnapshot.size() +
                " nonStaticFieldInfo.size() " + nonStaticFieldInfoMap.size() +
                " staticFieldInfo.size() " + staticFieldInfoMap.size();
    }
}
