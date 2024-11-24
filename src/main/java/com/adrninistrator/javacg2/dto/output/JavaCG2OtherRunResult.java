package com.adrninistrator.javacg2.dto.output;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/11/4
 * @description: 执行结果
 */
public class JavaCG2OtherRunResult {

    // 栈桢信息快照数量超过允许的最大数量的方法Set
    private final Set<String> frameSnapshotNumExceedMethodSet = new HashSet<>();

    public void addFrameSnapshotNumExceedMethod(String frameSnapshotNumExceedMethod) {
        frameSnapshotNumExceedMethodSet.add(frameSnapshotNumExceedMethod);
    }

    public void addFrameSnapshotNumExceedMethodSet(Set<String> frameSnapshotNumExceedMethodSet) {
        this.frameSnapshotNumExceedMethodSet.addAll(frameSnapshotNumExceedMethodSet);
    }

    public Set<String> getFrameSnapshotNumExceedMethodSetReadOnly() {
        return Collections.unmodifiableSet(frameSnapshotNumExceedMethodSet);
    }
}
