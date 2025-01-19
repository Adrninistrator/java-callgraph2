package com.adrninistrator.javacg2.dto.call;

import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.dto.element.variable.FieldElement;
import com.adrninistrator.javacg2.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg2.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg2.dto.element.variable.StaticFieldMethodCallElement;
import com.adrninistrator.javacg2.dto.element.variable.VariableElement;
import com.adrninistrator.javacg2.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg2.dto.field.StaticFieldTypeAndName;
import com.adrninistrator.javacg2.dto.variabledatasource.AbstractVariableDataSource;
import com.adrninistrator.javacg2.dto.variabledatasource.VariableDataSourceMethodArg;
import com.adrninistrator.javacg2.dto.variabledatasource.VariableDataSourceMethodCallReturn;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/11/3
 * @description: 方法调用中被调用对象、参数的可能的信息列表
 */
public class MethodCallPossibleList {

    private static final Logger logger = LoggerFactory.getLogger(MethodCallPossibleList.class);

    // 方法调用中可能的信息列表
    private List<MethodCallPossibleEntry> methodCallPossibleEntryList;

    // 可能的被调用非静态字段数量
    private int nonStaticFieldNum;

    // 可能的类型数量
    private int typeNum;

    // 是否为数组类型
    private boolean arrayElement;

    /**
     * 添加可能的信息
     *
     * @param baseElement        操作数栈中的元素
     * @param definedType        代码中定义的类型
     * @param handledElementList 已经处理过的元素
     */
    public void addPossibleInfo(BaseElement baseElement, String definedType, List<BaseElement> handledElementList) {
        if (baseElement == null) {
            return;
        }

        // 记录已经处理过的元素
        handledElementList.add(baseElement);
        String type = baseElement.getType();

        MethodCallPossibleEntry addedMethodCallPossibleEntry = new MethodCallPossibleEntry();
        if (baseElement instanceof StaticFieldMethodCallElement) {
            // 添加被调用对象或参数是静态字段方法返回值的可能信息
            String staticFieldMethodCallInfo = ((StaticFieldMethodCallElement) baseElement).getInfo();
            addedMethodCallPossibleEntry.setStaticFieldMethodCall(staticFieldMethodCallInfo);
        } else if (baseElement instanceof StaticFieldElement) {
            // 添加可能的被调用静态字段
            StaticFieldElement staticFieldElement = (StaticFieldElement) baseElement;
            StaticFieldTypeAndName staticField = new StaticFieldTypeAndName(type, staticFieldElement.getName(), staticFieldElement.getClassName());
            addedMethodCallPossibleEntry.setStaticField(staticField);
        } else if (baseElement instanceof FieldElement) {
            // 添加可能的被调用非静态字段
            FieldElement fieldElement = (FieldElement) baseElement;
            FieldTypeAndName fieldTypeAndName = new FieldTypeAndName(fieldElement.getType(), fieldElement.getName());
            addedMethodCallPossibleEntry.setNonStaticField(fieldTypeAndName);
        } else if (baseElement instanceof LocalVariableElement) {
            // 添加可能的被调用本地变量
            addedMethodCallPossibleEntry.setNameOfVariable(((LocalVariableElement) baseElement).getName());
        }
        // 以上有VariableElement的子类，这里不else
        if (baseElement instanceof VariableElement) {
            // 添加可能的被调用变量对应的数据来源
            VariableElement variableElement = (VariableElement) baseElement;
            AbstractVariableDataSource variableDataSource = variableElement.getVariableDataSource();
            if (variableDataSource instanceof VariableDataSourceMethodCallReturn) {
                VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) variableDataSource;
                String calleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(variableDataSourceMethodCallReturn.getCalleeClassName(),
                        variableDataSourceMethodCallReturn.getCalleeMethodName(), variableDataSourceMethodCallReturn.getCalleeArgTypeStr());
                addedMethodCallPossibleEntry.setMethodCallReturnFullMethod(calleeFullMethod);
                addedMethodCallPossibleEntry.setMethodCallReturnInstructionPosition(variableDataSourceMethodCallReturn.getInvokeInstructionPosition());
            } else if (variableDataSource instanceof VariableDataSourceMethodArg) {
                VariableDataSourceMethodArg variableDataSourceMethodArg = (VariableDataSourceMethodArg) variableDataSource;
                addedMethodCallPossibleEntry.setMethodArgSeq(variableDataSourceMethodArg.getArgSeq());
            }

            // 添加可能的被调用变量对应的等值转换前的数据来源
            AbstractVariableDataSource variableDataSourceEQC = variableElement.getVariableDataSourceEQC();
            if (variableDataSourceEQC instanceof VariableDataSourceMethodCallReturn) {
                VariableDataSourceMethodCallReturn variableDataSourceMethodCallEQC = (VariableDataSourceMethodCallReturn) variableDataSourceEQC;
                String calleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(variableDataSourceMethodCallEQC.getCalleeClassName(),
                        variableDataSourceMethodCallEQC.getCalleeMethodName(), variableDataSourceMethodCallEQC.getCalleeArgTypeStr());
                addedMethodCallPossibleEntry.setMethodCallReturnFullMethodEQC(calleeFullMethod);
                addedMethodCallPossibleEntry.setMethodCallReturnInstructionPositionEQC(variableDataSourceMethodCallEQC.getInvokeInstructionPosition());
            } else if (variableDataSourceEQC instanceof VariableDataSourceMethodArg) {
                VariableDataSourceMethodArg variableDataSourceMethodArgEQC = (VariableDataSourceMethodArg) variableDataSourceEQC;
                addedMethodCallPossibleEntry.setMethodArgSeqEQC(variableDataSourceMethodArgEQC.getArgSeq());
            }

            // 添加catch异常对象对应的catch代码块开始指令偏移量
            Integer catchExceptionStartPosition = variableElement.getCatchExceptionStartPosition();
            if (catchExceptionStartPosition != null) {
                addedMethodCallPossibleEntry.setCatchExceptionStartPosition(catchExceptionStartPosition);
            }
        }

        if (!JavaCG2ByteCodeUtil.isNullType(type) &&
                !type.equals(definedType) &&
                !JavaCG2ByteCodeUtil.compareIntType(type, definedType) &&
                !JavaCG2ByteCodeUtil.compareByteBooleanType(type, definedType)
        ) {
            /*
                添加可能的类型
                若操作数栈中元素的类型与字节码中的类型相同，则不添加
                若为int及兼容的类型，则不添加
                若为兼容的byte、boolean类型，则不添加
             */
            addedMethodCallPossibleEntry.setType(type);
        }

        Object value = baseElement.getValue();
        if (value != null) {
            // 添加可能的值
            addedMethodCallPossibleEntry.setValueType(type);
            addedMethodCallPossibleEntry.setValue(value);
        }

        if (addedMethodCallPossibleEntry.hasContent()) {
            // 包含内容，需要尝试添加
            if (methodCallPossibleEntryList == null) {
                methodCallPossibleEntryList = new ArrayList<>();
            }

            // 判断当前需要添加的信息是否已添加过相同的，若未添加过则添加
            boolean existed = false;
            for (MethodCallPossibleEntry existedMethodCallPossibleEntry : methodCallPossibleEntryList) {
                if (existedMethodCallPossibleEntry.compare(addedMethodCallPossibleEntry)) {
                    existed = true;
                    break;
                }
            }
            if (!existed) {
                // 添加记录
                addEntry(addedMethodCallPossibleEntry);
            }
        }

        // 判断是否属于数组
        if (baseElement.checkArrayElement()) {
            arrayElement = true;
            // 处理数组中的元素
            Map<Integer, BaseElement> map = baseElement.getArrayValueMap();
            if (!map.isEmpty()) {
                List<Integer> keyList = new ArrayList<>(map.keySet());
                Collections.sort(keyList);
                for (Integer key : keyList) {
                    BaseElement arrayElement = map.get(key);
                    if (handledElementList.contains(arrayElement)) {
                        // 假如数组中的元素已经处理过，说明出现了数组元素的循环引用，需要结束，否则会死循环
                        logger.warn("出现数组元素的循环引用 {}", arrayElement.getType());
                        continue;
                    }

                    String arrayElementType;
                    if (JavaCG2ByteCodeUtil.isArrayType(definedType)) {
                        /*
                            仅当参数类型属于数组时，才尝试去掉后面的[]
                            例如方法中参数定义的类型为Object，实际调用时指定的参数类型为Object[]
                         */
                        arrayElementType = JavaCG2ByteCodeUtil.removeOneArrayFlag(definedType);
                    } else {
                        arrayElementType = definedType;
                    }
                    // 处理数组中的元素
                    addPossibleInfo(arrayElement, arrayElementType, handledElementList);
                }
            }
        }
    }

    // 添加记录
    private void addEntry(MethodCallPossibleEntry addedMethodCallPossibleEntry) {
        methodCallPossibleEntryList.add(addedMethodCallPossibleEntry);
        // 记录内容数量
        if (addedMethodCallPossibleEntry.getNonStaticField() != null) {
            nonStaticFieldNum++;
        }
        if (addedMethodCallPossibleEntry.getType() != null) {
            typeNum++;
        }
    }

    public boolean hasNonStaticField() {
        return nonStaticFieldNum > 0;
    }

    public boolean hasType() {
        return typeNum > 0;
    }

    public List<MethodCallPossibleEntry> getMethodCallPossibleEntryList() {
        return methodCallPossibleEntryList;
    }

    public boolean isArrayElement() {
        return arrayElement;
    }
}
