package com.adrninistrator.javacg.dto.frame;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGElementUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/10/20
 * @description:
 */
public class JavaCGLocalVariables {

    private List<LocalVariableElement> localVariableElementList;

    /**
     * 初始化方法的本地变量
     *
     * @param mg
     * @return
     */
    public JavaCGLocalVariables(MethodGen mg) {
        localVariableElementList = new ArrayList<>(mg.getMaxLocals());

        int index = 0;
        if (!mg.isStatic()) {
            // 非静态方法，将this加入本地变量
            LocalVariableElement thisLocalVariableElement = new LocalVariableElement(mg.getClassName(), null, index);
            localVariableElementList.add(thisLocalVariableElement);
            index++;
        }

        // 将参数加入本地变量
        for (Type arg : mg.getArgumentTypes()) {
            LocalVariableElement localVariableElement = new LocalVariableElement(arg.toString(), null, index);
            localVariableElementList.add(localVariableElement);
            index++;

            if (localVariableElement.getElementSize() == 2) {
                localVariableElementList.add(null);
                index++;
            }
        }
    }

    /**
     * 拷贝所有的本地变量
     *
     * @return
     */
    public JavaCGLocalVariables copy() {
        JavaCGLocalVariables clone = new JavaCGLocalVariables();
        clone.localVariableElementList = new ArrayList<>(localVariableElementList);
        return clone;
    }

    /**
     * 比较本地变量，宽松模式
     *
     * @param existed
     * @param added
     * @param sameLocalsSeqSet
     */
    public static void compareLooseMode(JavaCGLocalVariables existed, JavaCGLocalVariables added, Set<Integer> sameLocalsSeqSet) {
        if (added == null) {
            return;
        }

        int size = existed.localVariableElementList.size();
        if (size != added.size()) {
            return;
        }

        for (int i = 0; i < size; i++) {
            LocalVariableElement existedLocalVariable = existed.localVariableElementList.get(i);
            LocalVariableElement addedLocalVariable = added.localVariableElementList.get(i);

            if (addedLocalVariable == null ||
                    (existedLocalVariable != null && JavaCGElementUtil.compare(existedLocalVariable, addedLocalVariable))) {
                sameLocalsSeqSet.add(i);
            }
        }
    }

    public int size() {
        return localVariableElementList.size();
    }

    public LocalVariableElement get(int index) {
        return localVariableElementList.get(index);
    }

    /**
     * 添加本地变量
     *
     * @param type        本地变量的类型
     * @param baseElement 操作数栈出栈的内容
     * @param index       本地变量的索引
     */
    public void add(String type, BaseElement baseElement, int index) {
        LocalVariableElement localVariableElement;
        if (baseElement instanceof StaticFieldElement) {
            StaticFieldElement staticFieldElement = (StaticFieldElement) baseElement;
            localVariableElement = new StaticFieldElement(type, baseElement.getValue(), staticFieldElement.getFieldName(), staticFieldElement.getClassName(), index);
        } else if (baseElement instanceof FieldElement) {
            FieldElement fieldElement = (FieldElement) baseElement;
            localVariableElement = new FieldElement(type, baseElement.getValue(), fieldElement.getFieldName(), index);
        } else {
            localVariableElement = new LocalVariableElement(type, baseElement.getValue(), index);
        }
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("### 添加本地变量 (" + index + ") " + localVariableElement);
        }

        if (localVariableElementList.size() > index) {
            // 对应索引的本地变量已存在
            // 记录本地变量
            localVariableElementList.set(index, localVariableElement);
            return;
        }

        // 对应索引的本地变量不存在
        // 中间间隔的位置添加null值
        int nullElementNum = index - localVariableElementList.size();
        for (int i = 0; i < nullElementNum; i++) {
            localVariableElementList.add(null);
        }

        // 记录本地变量
        localVariableElementList.add(localVariableElement);
        if (JavaCGByteCodeUtil.getTypeSize(type) == 2) {
            localVariableElementList.add(null);
        }
    }

    /**
     * 将指定的本地变量值设为null
     *
     * @param index
     */
    public void setValue2Null(int index) {
        LocalVariableElement localVariableElement = localVariableElementList.get(index);
        if (localVariableElement.getValue() == null) {
            return;
        }

        localVariableElementList.set(index, localVariableElement.copyWithNullValue());
    }

    private JavaCGLocalVariables() {
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("num: ").append(localVariableElementList.size()).append("\n");

        for (int i = 0; i < localVariableElementList.size(); i++) {
            LocalVariableElement localVariableElement = localVariableElementList.get(i);
            stringBuilder.append(i).append(" ");
            if (localVariableElement != null) {
                stringBuilder.append(localVariableElement);
            } else {
                stringBuilder.append("null");
            }
            stringBuilder.append("\n");
        }

        return stringBuilder.toString();
    }
}
