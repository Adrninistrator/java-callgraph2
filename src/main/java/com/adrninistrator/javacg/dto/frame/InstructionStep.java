package com.adrninistrator.javacg.dto.frame;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/10/28
 * @description: 指令执行的步骤
 */
public class InstructionStep {

    private List<String> stepList = new ArrayList<>();

    /**
     * 记录指令执行的步骤
     *
     * @param instructionInfo 指令信息
     * @param lineNumber      代码行号
     */
    public void add(String instructionInfo, int lineNumber) {
        stepList.add(instructionInfo + " (" + lineNumber + ")");
    }

    public void clear() {
        stepList.clear();
    }

    public InstructionStep copy() {
        InstructionStep clone = new InstructionStep();
        clone.stepList = new ArrayList<>(stepList);
        return clone;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        for (String step : stepList) {
            stringBuilder.append(step).append("\n");
        }
        return stringBuilder.toString();
    }
}
