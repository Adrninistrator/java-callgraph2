package com.adrninistrator.javacg.dto.frame;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg.dto.element.variable.VariableElement;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGElementUtil;
import org.apache.bcel.classfile.LocalVariable;
import org.apache.bcel.classfile.LocalVariableTable;
import org.apache.bcel.generic.ArrayType;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.codec.digest.DigestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/10/20
 * @description:
 */
public class JavaCGLocalVariables {

    private static final Logger logger = LoggerFactory.getLogger(JavaCGLocalVariables.class);

    private List<LocalVariableElement> localVariableElementList;

    private JavaCGLocalVariables() {
    }

    /**
     * 初始化方法的本地变量
     *
     * @param mg
     * @return
     */
    public JavaCGLocalVariables(LocalVariableTable localVariableTable, MethodGen mg) {
        localVariableElementList = new ArrayList<>(mg.getMaxLocals());

        int index = 0;
        if (!mg.isStatic()) {
            // 非静态方法，将this加入本地变量
            LocalVariableElement thisLocalVariableElement = new LocalVariableElement(mg.getClassName(), false, null, index, JavaCGConstants.THIS);
            localVariableElementList.add(thisLocalVariableElement);
            index++;
        }

        // 将参数加入本地变量
        for (Type arg : mg.getArgumentTypes()) {
            // 获取参数对应的本地变量值，pc使用0就能获取到
            LocalVariable localVariable = localVariableTable.getLocalVariable(index, 0);
            LocalVariableElement localVariableElement = new LocalVariableElement(arg.toString(), (arg instanceof ArrayType), null, index,
                    (localVariable != null) ? localVariable.getName() : null);
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
        JavaCGLocalVariables javaCGLocalVariablesCopy = new JavaCGLocalVariables();
        javaCGLocalVariablesCopy.localVariableElementList = new ArrayList<>(this.localVariableElementList.size());
        for (LocalVariableElement localVariableElement : this.localVariableElementList) {
            if (localVariableElement == null) {
                javaCGLocalVariablesCopy.localVariableElementList.add(null);
                continue;
            }
            javaCGLocalVariablesCopy.localVariableElementList.add((LocalVariableElement)localVariableElement.copyElement());
        }
        return javaCGLocalVariablesCopy;
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
    public void add(String type, BaseElement baseElement, int index, String variableName) {
        LocalVariableElement localVariableElement;
        if (baseElement instanceof StaticFieldElement) {
            StaticFieldElement staticFieldElement = (StaticFieldElement) baseElement;
            localVariableElement = new StaticFieldElement(type, staticFieldElement.isArrayElement(), staticFieldElement.getValue(), index, staticFieldElement.getName(),
                    staticFieldElement.getClassName());
        } else if (baseElement instanceof FieldElement) {
            FieldElement fieldElement = (FieldElement) baseElement;
            localVariableElement = new FieldElement(type, fieldElement.isArrayElement(), fieldElement.getValue(), index, fieldElement.getName(), fieldElement.getClassName());
        } else {
            localVariableElement = new LocalVariableElement(type, baseElement.isArrayElement(), baseElement.getValue(), index, variableName);
            // 非FieldElement，且非StaticFieldElement时，拷贝数据来源
            localVariableElement.copyVariableDataSource(baseElement);
        }
        if (baseElement instanceof VariableElement) {
            VariableElement variableElement = (VariableElement) baseElement;
            localVariableElement.setCatchExceptionStartPosition(variableElement.getCatchExceptionStartPosition());
        }

        // 数组类型的处理
        if (baseElement.isArrayElement()) {
            localVariableElement.setArrayValueMap(baseElement.getArrayValueMap());
        }

        logger.debug("添加本地变量 ({}) {}", index, localVariableElement);

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

    /**
     * 生成内容的HASH值
     *
     * @return
     */
    public String genHash() {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < localVariableElementList.size(); i++) {
            LocalVariableElement variableElement = localVariableElementList.get(i);
            stringBuilder.append(variableElement).append("\n");
        }
        return DigestUtils.md5Hex(stringBuilder.toString());
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("hash: ").append(genHash()).append("\n");
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
