package com.adrninistrator.javacg.dto.frame;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/10/21
 * @description: 指令与对应的栈桢信息快照列表
 */
public class FrameSnapshotsOfIhs {

    /*
        指令对应的栈桢信息快照
        key
            指令位置
        value
            栈桢信息快照列表
     */
    private final Map<Integer, List<FrameSnapshotEntry>> frameSnapshotOfIhsMap;

    public FrameSnapshotsOfIhs() {
        frameSnapshotOfIhsMap = new HashMap<>();
    }

    public List<FrameSnapshotEntry> get(Integer position) {
        return frameSnapshotOfIhsMap.get(position);
    }

    /**
     * 添加信息快照
     *
     * @param position
     * @param addedStack
     * @param addedLocals
     * @param addedNonStaticFieldInfo
     * @param addedStaticFieldInfo
     * @return false: 已存在相同的信息快照，未添加 true: 不存在相同的信息快照，有添加
     */
    public boolean addSnapshot(int position,
                               JavaCGOperandStack addedStack,
                               JavaCGLocalVariables addedLocals,
                               FieldInformation addedNonStaticFieldInfo,
                               FieldInformation addedStaticFieldInfo) {
        List<FrameSnapshotEntry> frameSnapshotEntryList = frameSnapshotOfIhsMap.get(position);
        if (frameSnapshotEntryList == null) {
            // 当前跳转目标指令处理过的栈桢信息快照为空，记录信息快照
            frameSnapshotEntryList = new ArrayList<>();
            frameSnapshotEntryList.add(new FrameSnapshotEntry(addedStack.copy(), addedLocals.copy(), addedNonStaticFieldInfo.copy(), addedStaticFieldInfo.copy()));
            frameSnapshotOfIhsMap.put(position, frameSnapshotEntryList);
            return true;
        }

        // 判断当前栈桢对应的信息快照是否存在相同的记录
        if (checkExistSnapshotLooseMode(frameSnapshotEntryList, addedStack, addedLocals, addedNonStaticFieldInfo, addedStaticFieldInfo)) {
            // 已存在相同的信息快照，未添加
            return false;
        }

        // 不存在相同的信息快照，需要添加
        frameSnapshotEntryList.add(new FrameSnapshotEntry(addedStack.copy(), addedLocals.copy(), addedNonStaticFieldInfo.copy(), addedStaticFieldInfo.copy()));
        return true;
    }

    /**
     * 判断当前栈桢对应的信息快照是否存在相同的记录，宽松模式
     *
     * @param frameSnapshotEntryList
     * @param addedStack
     * @param addedLocals
     * @param addedNonStaticFieldInfo
     * @param addedStaticFieldInfo
     * @return false: 不存在相同的信息快照 true: 已存在相同的信息快照
     */
    private boolean checkExistSnapshotLooseMode(List<FrameSnapshotEntry> frameSnapshotEntryList,
                                                JavaCGOperandStack addedStack,
                                                JavaCGLocalVariables addedLocals,
                                                FieldInformation addedNonStaticFieldInfo,
                                                FieldInformation addedStaticFieldInfo) {
        int addedStackSize = addedStack.size();
        int addedLocalsSize = addedLocals.size();
        int addedNonStaticFieldInfoSize = addedNonStaticFieldInfo.size();
        int addedStaticFieldInfoSize = addedStaticFieldInfo.size();

        Set<Integer> sameStackSeqSet = new HashSet<>(addedStackSize);
        Set<Integer> sameLocalsSeqSet = new HashSet<>(addedLocalsSize);
        Set<String> sameNonStaticFieldInfoNameSet = new HashSet<>(addedNonStaticFieldInfoSize);
        Set<String> sameStaticFieldInfoNameSet = new HashSet<>(addedStaticFieldInfoSize);

        for (FrameSnapshotEntry frameSnapshotEntry : frameSnapshotEntryList) {
            /*
                以下处理时，每种类型都需要判断数量是否已相等，若不相等则需要进行比较
                进行比较后，可能需要添加的数据已存在，需要再判断数量是否已相等，若还是不相等才能认为暂不存在与需要添加的相同的数据
             */
            boolean equals = true;

            if (addedStackSize != sameStackSeqSet.size()) {
                // 操作数栈还不是每个元素都存在相同的信息，还需要比较
                JavaCGOperandStack.compareLooseMode(frameSnapshotEntry.getStackSnapshot(), addedStack, sameStackSeqSet);
                if (addedStackSize != sameStackSeqSet.size()) {
                    equals = false;
                }
            }

            if (addedLocalsSize != sameLocalsSeqSet.size()) {
                // 本地变量还不是每个元素都存在相同的信息，还需要比较
                JavaCGLocalVariables.compareLooseMode(frameSnapshotEntry.getLocalsSnapshot(), addedLocals, sameLocalsSeqSet);
                if (addedLocalsSize != sameLocalsSeqSet.size()) {
                    equals = false;
                }
            }

            if (addedNonStaticFieldInfoSize != sameNonStaticFieldInfoNameSet.size()) {
                // 本地变量还不是每个元素都存在相同的信息，还需要比较
                FieldInformation.compareLooseMode(frameSnapshotEntry.getNonStaticFieldInfo(), addedNonStaticFieldInfo, sameNonStaticFieldInfoNameSet);
                if (addedNonStaticFieldInfoSize != sameNonStaticFieldInfoNameSet.size()) {
                    equals = false;
                }
            }

            if (addedStaticFieldInfoSize != sameStaticFieldInfoNameSet.size()) {
                // 本地变量还不是每个元素都存在相同的信息，还需要比较
                FieldInformation.compareLooseMode(frameSnapshotEntry.getStaticFieldInfo(), addedStaticFieldInfo, sameStaticFieldInfoNameSet);
                if (addedStaticFieldInfoSize != sameStaticFieldInfoNameSet.size()) {
                    equals = false;
                }
            }

            if (equals) {
                // 已存在相同的信息快照
                return true;
            }
        }

        // 不存在相同的信息快照
        return false;
    }
}
