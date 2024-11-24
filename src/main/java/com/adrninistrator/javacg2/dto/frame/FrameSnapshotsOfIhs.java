package com.adrninistrator.javacg2.dto.frame;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    private static final Logger logger = LoggerFactory.getLogger(FrameSnapshotsOfIhs.class);

    private final String fullMethod;

    private final String usage;

    private final Set<String> frameSnapshotNumExceedMethodSet;

    /*
        指令对应的栈桢信息快照
        key
            指令位置
        value
            栈桢信息快照列表
     */
    private final Map<Integer, List<FrameSnapshotEntry>> frameSnapshotOfIhsMap;

    // 栈桢快照信息数量
    private int frameSnapshotNum = 0;

    // 栈桢快照信息数量是否超过允许的最大值
    private boolean exceedFrameSnapshotNum = false;

    public FrameSnapshotsOfIhs(String fullMethod, String usage, Set<String> frameSnapshotNumExceedMethodSet) {
        this.fullMethod = fullMethod;
        this.usage = usage;
        this.frameSnapshotNumExceedMethodSet = frameSnapshotNumExceedMethodSet;
        frameSnapshotOfIhsMap = new HashMap<>();
    }

    public List<FrameSnapshotEntry> get(Integer position) {
        return frameSnapshotOfIhsMap.get(position);
    }

    /**
     * 添加栈桢信息快照
     *
     * @param position
     * @param addedStack
     * @param addedLocals
     * @param addedNonStaticFieldInfoMap
     * @param addedStaticFieldInfoMap
     * @return false: 已存在相同的信息快照，未添加 true: 不存在相同的信息快照，有添加
     */
    public boolean addSnapshot(int position,
                               JavaCG2OperandStack addedStack,
                               JavaCG2LocalVariables addedLocals,
                               FieldInformationMap addedNonStaticFieldInfoMap,
                               FieldInformationMap addedStaticFieldInfoMap) {
        if (exceedFrameSnapshotNum) {
            // 方法的栈桢信息快照数量超过允许的最大数量，固定返回已存在相同的信息快照
            return false;
        }

        List<FrameSnapshotEntry> frameSnapshotEntryList = frameSnapshotOfIhsMap.get(position);
        if (frameSnapshotEntryList == null) {
            // 当前跳转目标指令处理过的栈桢信息快照为空，记录信息快照
            frameSnapshotEntryList = new ArrayList<>();
            frameSnapshotEntryList.add(new FrameSnapshotEntry(addedStack.copy(), addedLocals.copy(), addedNonStaticFieldInfoMap.copy(), addedStaticFieldInfoMap.copy()));
            frameSnapshotOfIhsMap.put(position, frameSnapshotEntryList);
            // 处理栈桢信息快照数量
            handleFrameSnapshotEntryNum();
            return true;
        }

        // 判断当前栈桢对应的信息快照是否存在相同的记录
        if (checkExistSnapshotLooseMode(frameSnapshotEntryList, addedStack, addedLocals, addedNonStaticFieldInfoMap, addedStaticFieldInfoMap)) {
            // 已存在相同的信息快照，不需要添加
            return false;
        }

        // 不存在相同的信息快照，需要添加
        frameSnapshotEntryList.add(new FrameSnapshotEntry(addedStack.copy(), addedLocals.copy(), addedNonStaticFieldInfoMap.copy(), addedStaticFieldInfoMap.copy()));
        // 处理栈桢信息快照数量
        handleFrameSnapshotEntryNum();
        return true;
    }

    // 处理栈桢信息快照数量
    private void handleFrameSnapshotEntryNum() {
        frameSnapshotNum++;

        if (frameSnapshotNum >= JavaCG2Constants.MAX_FRAME_SNAP_SHOTS_NUM) {
            logger.warn("方法的栈桢信息快照数量超过允许的最大数量 {} {} {}", usage, fullMethod, frameSnapshotNum);
            exceedFrameSnapshotNum = true;
            frameSnapshotNumExceedMethodSet.add(fullMethod);
        }
    }

    /**
     * 判断当前栈桢对应的信息快照是否存在相同的记录，宽松模式
     *
     * @param frameSnapshotEntryList
     * @param addedStack
     * @param addedLocals
     * @param addedNonStaticFieldInfoMap
     * @param addedStaticFieldInfoMap
     * @return false: 不存在相同的信息快照 true: 已存在相同的信息快照
     */
    private boolean checkExistSnapshotLooseMode(List<FrameSnapshotEntry> frameSnapshotEntryList,
                                                JavaCG2OperandStack addedStack,
                                                JavaCG2LocalVariables addedLocals,
                                                FieldInformationMap addedNonStaticFieldInfoMap,
                                                FieldInformationMap addedStaticFieldInfoMap) {
        int addedStackSize = addedStack.size();
        int addedLocalsSize = addedLocals.size();
        int addedNonStaticFieldInfoSize = addedNonStaticFieldInfoMap.size();
        int addedStaticFieldInfoSize = addedStaticFieldInfoMap.size();

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
                JavaCG2OperandStack.compareLooseMode(frameSnapshotEntry.getStackSnapshot(), addedStack, sameStackSeqSet);
                if (addedStackSize != sameStackSeqSet.size()) {
                    equals = false;
                }
            }

            if (addedLocalsSize != sameLocalsSeqSet.size()) {
                // 本地变量还不是每个元素都存在相同的信息，还需要比较
                JavaCG2LocalVariables.compareLooseMode(frameSnapshotEntry.getLocalsSnapshot(), addedLocals, sameLocalsSeqSet);
                if (addedLocalsSize != sameLocalsSeqSet.size()) {
                    equals = false;
                }
            }

            if (addedNonStaticFieldInfoSize != sameNonStaticFieldInfoNameSet.size()) {
                // 本地变量还不是每个元素都存在相同的信息，还需要比较
                FieldInformationMap.compareLooseMode(frameSnapshotEntry.getNonStaticFieldInfoMap(), addedNonStaticFieldInfoMap, sameNonStaticFieldInfoNameSet);
                if (addedNonStaticFieldInfoSize != sameNonStaticFieldInfoNameSet.size()) {
                    equals = false;
                }
            }

            if (addedStaticFieldInfoSize != sameStaticFieldInfoNameSet.size()) {
                // 本地变量还不是每个元素都存在相同的信息，还需要比较
                FieldInformationMap.compareLooseMode(frameSnapshotEntry.getStaticFieldInfoMap(), addedStaticFieldInfoMap, sameStaticFieldInfoNameSet);
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
