package com.adrninistrator.javacg2.dto.frame;

import com.adrninistrator.javacg2.util.JavaCG2InstructionUtil;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/10/28
 * @description: 指令执行的步骤列表
 */
public class InstructionStepList {

    // 指令偏移量列表
    private List<Integer> positionList = new ArrayList<>();

    // 用于打印的指令信息列表
    private List<String> instructionPrintInfoList = new ArrayList<>();

    /**
     * 记录指令执行的步骤
     *
     * @param ih         指令对象
     * @param lineNumber 代码行号
     */
    public void add(InstructionHandle ih, int lineNumber) {
        String instructionInfo4Print = JavaCG2InstructionUtil.getInstructionHandlePrintInfo(ih) + " (" + lineNumber + ")";
        positionList.add(ih.getPosition());
        instructionPrintInfoList.add(instructionInfo4Print);
    }

    public void clear() {
        positionList.clear();
        instructionPrintInfoList.clear();
    }

    public InstructionStepList copy() {
        InstructionStepList instructionStepCopy = new InstructionStepList();
        instructionStepCopy.positionList = new ArrayList<>(this.positionList);
        instructionStepCopy.instructionPrintInfoList = new ArrayList<>(this.instructionPrintInfoList);
        return instructionStepCopy;
    }

    /**
     * 生成当前指令执行步骤的HASH
     *
     * @return
     */
    public String genInstructionStepHash() {
        String instructionPositionStr = StringUtils.join(positionList, " ");
        return DigestUtils.md5Hex(instructionPositionStr);
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        for (String instructionPrintInfo : instructionPrintInfoList) {
            stringBuilder.append(instructionPrintInfo).append("\n");
        }
        return stringBuilder.toString();
    }
}
