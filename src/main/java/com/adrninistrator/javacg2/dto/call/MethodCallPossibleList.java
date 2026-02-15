package com.adrninistrator.javacg2.dto.call;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.dto.element.variable.FieldElement;
import com.adrninistrator.javacg2.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg2.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg2.dto.element.variable.StaticFieldMethodCallReturnElement;
import com.adrninistrator.javacg2.dto.element.variable.VariableElement;
import com.adrninistrator.javacg2.dto.field.ClassFieldTypeAndName;
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

    // 数组元素的可能的信息组合列表，用于去重
    private List<List<MethodCallPossibleEntry>> arrayElementEntryListList;

    /**
     * 添加可能的信息
     *
     * @param baseElement        操作数栈中的元素
     * @param definedType        代码中定义的类型
     * @param handledElementList 已经处理过的元素
     */
    public void addPossibleInfo(BaseElement baseElement, String definedType, List<BaseElement> handledElementList) {
        addPossibleInfo(baseElement, definedType, handledElementList, null);
    }

    /**
     * 添加可能的信息（带数组索引前缀）
     *
     * @param baseElement        操作数栈中的元素
     * @param definedType        代码中定义的类型
     * @param handledElementList 已经处理过的元素
     * @param arrayIndexPrefix   数组索引前缀
     */
    public void addPossibleInfo(BaseElement baseElement, String definedType, List<BaseElement> handledElementList,
                             String arrayIndexPrefix) {
        if (baseElement == null) {
            return;
        }

        // 记录已经处理过的元素
        handledElementList.add(baseElement);
        String elementType = baseElement.getType();

        MethodCallPossibleEntry addedMethodCallPossibleEntry = new MethodCallPossibleEntry();

        // 记录数组索引信息
        if (arrayIndexPrefix != null) {
            addedMethodCallPossibleEntry.setArrayIndex(arrayIndexPrefix);
        }

        if (baseElement instanceof StaticFieldMethodCallReturnElement) {
            // 添加被调用对象或参数是静态字段方法返回值的可能信息
            String staticFieldMethodCallInfo = ((StaticFieldMethodCallReturnElement) baseElement).getDetailInfo();
            addedMethodCallPossibleEntry.setStaticFieldMethodCall(staticFieldMethodCallInfo);
        } else if (baseElement instanceof StaticFieldElement) {
            // 添加可能的被调用静态字段
            StaticFieldElement staticFieldElement = (StaticFieldElement) baseElement;
            ClassFieldTypeAndName staticField = new ClassFieldTypeAndName(staticFieldElement.getType(), staticFieldElement.getName(), staticFieldElement.getClassName());
            addedMethodCallPossibleEntry.setStaticField(staticField);
        } else if (baseElement instanceof FieldElement) {
            // 添加可能的被调用非静态字段
            FieldElement fieldElement = (FieldElement) baseElement;
            ClassFieldTypeAndName nonStaticField = new ClassFieldTypeAndName(fieldElement.getType(), fieldElement.getName(), fieldElement.getClassName());
            addedMethodCallPossibleEntry.setNonStaticField(nonStaticField);
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
                addedMethodCallPossibleEntry.setMethodCallReturnReturnType(variableDataSourceMethodCallReturn.getReturnType());
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
                addedMethodCallPossibleEntry.setMethodCallReturnReturnTypeEQC(variableDataSourceMethodCallEQC.getReturnType());
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

        // 当元素本身是数组类型时，不记录类型信息（数组的中间层元素不需要记录类型）
        boolean currentElementIsArray = baseElement.checkArrayElement();
        if (!currentElementIsArray &&
                !JavaCG2ByteCodeUtil.isNullType(elementType) &&
                !elementType.equals(definedType) &&
                !JavaCG2ByteCodeUtil.compareIntType(elementType, definedType) &&
                !JavaCG2ByteCodeUtil.compareByteBooleanType(elementType, definedType)
        ) {
            /*
                添加可能的类型
                若操作数栈中元素的类型与字节码中的类型相同，则不添加
                若为int及兼容的类型，则不添加
                若为兼容的byte、boolean类型，则不添加
                若当前元素是数组类型（数组的中间层），则不添加
             */
            addedMethodCallPossibleEntry.setType(elementType);
        }

        Object value = baseElement.getValue();
        if (value != null) {
            // 添加可能的值
            addedMethodCallPossibleEntry.setValueType(elementType);
            addedMethodCallPossibleEntry.setValue(value);
        }

        if (addedMethodCallPossibleEntry.hasContent()) {
            // 包含内容，需要尝试添加
            if (methodCallPossibleEntryList == null) {
                methodCallPossibleEntryList = new ArrayList<>();
            }

            boolean existed = false;
            if (!arrayElement) {
                // 不属于数组元素时，判断当前需要添加的信息是否已添加过相同的，若未添加过则添加
                for (MethodCallPossibleEntry existedMethodCallPossibleEntry : methodCallPossibleEntryList) {
                    if (existedMethodCallPossibleEntry.compare(addedMethodCallPossibleEntry)) {
                        existed = true;
                        break;
                    }
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
                // 临时保存当前处理的数组元素信息
                List<MethodCallPossibleEntry> currentArrayElementList = new ArrayList<>();
                // 获取数组元素信息列表的大小（处理数组元素之前）
                int entryListSizeBeforeArray = methodCallPossibleEntryList != null ? methodCallPossibleEntryList.size() : 0;

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

                    // 构造数组索引
                    String newArrayIndexPrefix = combineArrayIndex(arrayIndexPrefix, key);

                    // 处理数组中的元素
                    addPossibleInfo(arrayElement, arrayElementType, handledElementList, newArrayIndexPrefix);
                }

                // 收集新添加的数组元素信息
                int entryListSizeAfterArray = methodCallPossibleEntryList != null ? methodCallPossibleEntryList.size() : 0;
                for (int i = entryListSizeBeforeArray; i < entryListSizeAfterArray; i++) {
                    currentArrayElementList.add(methodCallPossibleEntryList.get(i));
                }

                // 查找或确定数组元素组合的序号，并处理重复
                int collectionSeq = findOrAddArrayElementCollectionSeq(currentArrayElementList, entryListSizeBeforeArray, entryListSizeAfterArray);

                // 为当前保留的数组元素组合中的每个 entry 设置组合序号
                // 注意：如果组合已存在，entry 可能已被移除，此时需要跳过
                if (collectionSeq >= 0 && methodCallPossibleEntryList != null) {
                    for (int i = entryListSizeBeforeArray; i < methodCallPossibleEntryList.size(); i++) {
                        methodCallPossibleEntryList.get(i).setArrayCollectionSeq(collectionSeq);
                    }
                }
            }
        }
    }

    /**
     * 查找或添加数组元素组合的序号
     *
     * @param currentArrayElementList      当前数组元素组合
     * @param entryListSizeBeforeArray     处理数组元素前列表大小
     * @param entryListSizeAfterArray      处理数组元素后列表大小
     * @return 组合序号；如果组合已存在并被移除，返回已存在的序号（负值表示需要跳过设置）
     */
    private int findOrAddArrayElementCollectionSeq(List<MethodCallPossibleEntry> currentArrayElementList,
                                                   int entryListSizeBeforeArray, int entryListSizeAfterArray) {
        if (arrayElementEntryListList == null) {
            arrayElementEntryListList = new ArrayList<>();
        }

        // 查找是否已存在相同的组合
        for (int i = 0; i < arrayElementEntryListList.size(); i++) {
            if (compareArrayElementList(currentArrayElementList, arrayElementEntryListList.get(i))) {
                // 找到已存在的组合，移除当前重复的元素
                if (methodCallPossibleEntryList != null) {
                    for (int j = entryListSizeAfterArray - 1; j >= entryListSizeBeforeArray; j--) {
                        MethodCallPossibleEntry removedEntry = methodCallPossibleEntryList.remove(j);
                        // 恢复内容数量
                        if (removedEntry.getNonStaticField() != null) {
                            nonStaticFieldNum--;
                        }
                        if (removedEntry.getType() != null) {
                            typeNum--;
                        }
                    }
                }
                // 返回负值表示组合已存在，entry 已被移除
                return -i - 1;
            }
        }

        // 未找到相同组合，添加新组合
        arrayElementEntryListList.add(currentArrayElementList);
        return arrayElementEntryListList.size() - 1;
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

    /**
     * 获取数组维度
     *
     * @return 数组维度，0代表不是数组，1代表一维数组，2代表二维数组...
     */
    public int getArrayDimensions() {
        if (!arrayElement) {
            return 0;
        }

        // 遍历所有 entry，找到最大的数组索引长度
        int maxDimensions = 0;
        if (methodCallPossibleEntryList != null) {
            for (MethodCallPossibleEntry entry : methodCallPossibleEntryList) {
                String arrayIndex = entry.getArrayIndex();
                if (arrayIndex != null && !arrayIndex.isEmpty()) {
                    // 通过逗号数量计算维度（逗号数量 + 1）
                    int dimensions = getArrayIndexDimensions(arrayIndex);
                    if (dimensions > maxDimensions) {
                        maxDimensions = dimensions;
                    }
                }
            }
        }
        return maxDimensions;
    }

    /**
     * 比较两个数组元素列表是否相同
     *
     * @param list1 数组元素列表1
     * @param list2 数组元素列表2
     * @return true: 相同；false: 不同
     */
    private boolean compareArrayElementList(List<MethodCallPossibleEntry> list1, List<MethodCallPossibleEntry> list2) {
        if (list1 == null && list2 == null) {
            return true;
        }
        if (list1 == null || list2 == null) {
            return false;
        }
        if (list1.size() != list2.size()) {
            return false;
        }

        for (int i = 0; i < list1.size(); i++) {
            MethodCallPossibleEntry entry1 = list1.get(i);
            MethodCallPossibleEntry entry2 = list2.get(i);
            if (!entry1.compare(entry2)) {
                return false;
            }
        }
        return true;
    }

    /**
     * 拼接数组索引
     *
     * @param prefix 索引前缀，可为null
     * @param index  当前索引
     * @return 拼接后的索引字符串，如 "0", "0,1", "0,1,2"
     */
    public static String combineArrayIndex(String prefix, int index) {
        if (prefix == null) {
            return String.valueOf(index);
        }
        return prefix + JavaCG2Constants.FLAG_COMMA + index;
    }

    /**
     * 根据数组索引字符串计算维度
     *
     * @param arrayIndex 数组索引字符串，如 "0" 返回1，"0,1" 返回2，"0,1,2" 返回3
     * @return 数组维度
     */
    public static int getArrayIndexDimensions(String arrayIndex) {
        if (arrayIndex == null || arrayIndex.isEmpty()) {
            return 0;
        }
        // 通过逗号数量计算维度（逗号数量 + 1）
        return arrayIndex.split(JavaCG2Constants.FLAG_COMMA).length;
    }
}
