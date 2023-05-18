package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldMethodCallElement;
import com.adrninistrator.javacg.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;

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

    // 方法调用中可能的信息列表
    private List<MethodCallPossibleEntry> methodCallPossibleEntryList;

    // 可能的被调用静态字段数量
    private int staticFieldClassAndFieldNameNum;

    // 可能的被调用非静态字段数量
    private int nonStaticFieldNum;

    // 可能的类型数量
    private int typeNum;

    // 可能的值数量
    private int valueNum;

    // 被调用对象或参数是静态字段方法返回值的可能信息数量
    private int staticFieldMethodCallNum;

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

        MethodCallPossibleEntry addedMethodCallPossibleEntry = new MethodCallPossibleEntry();
        if (baseElement instanceof StaticFieldMethodCallElement) {
            // 添加被调用对象或参数是静态字段方法返回值的可能信息
            String staticFieldMethodCallInfo = ((StaticFieldMethodCallElement) baseElement).getInfo();
            addedMethodCallPossibleEntry.setStaticFieldMethodCall(staticFieldMethodCallInfo);
        } else if (baseElement instanceof StaticFieldElement) {
            // 添加可能的被调用静态字段
            String staticFieldClassAndFieldName = ((StaticFieldElement) baseElement).getClassAndFieldName();
            addedMethodCallPossibleEntry.setStaticFieldClassAndFieldName(staticFieldClassAndFieldName);
        } else if (baseElement instanceof FieldElement) {
            // 添加可能的被调用非静态字段
            FieldElement fieldElement = (FieldElement) baseElement;
            FieldTypeAndName fieldTypeAndName = new FieldTypeAndName(fieldElement.getType(), fieldElement.getFieldName());
            addedMethodCallPossibleEntry.setNonStaticField(fieldTypeAndName);
        }

        String type = baseElement.getType();
        if (!JavaCGByteCodeUtil.isNullType(type) &&
                !type.equals(definedType) &&
                !JavaCGByteCodeUtil.compareIntType(type, definedType) &&
                !JavaCGByteCodeUtil.compareByteBooleanType(type, definedType)
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
        if (baseElement.isArrayElement()) {
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
                        System.err.println("eee 出现数组元素的循环引用");
                        if (JavaCGLogUtil.isDebugPrintFlag()) {
                            JavaCGLogUtil.debugPrint("eee 出现数组元素的循环引用 " + arrayElement);
                        }
                        continue;
                    }

                    String arrayElementType;
                    if (JavaCGByteCodeUtil.isArrayType(definedType)) {
                        /*
                            仅当参数类型属于数组时，才尝试去掉后面的[]
                            例如方法中参数定义的类型为Object，实际调用时指定的参数类型为Object[]
                         */
                        arrayElementType = JavaCGByteCodeUtil.removeArrayFlag(definedType);
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
        if (addedMethodCallPossibleEntry.getStaticFieldMethodCall() != null) {
            staticFieldMethodCallNum++;
        }
        if (addedMethodCallPossibleEntry.getStaticFieldClassAndFieldName() != null) {
            staticFieldClassAndFieldNameNum++;
        }
        if (addedMethodCallPossibleEntry.getNonStaticField() != null) {
            nonStaticFieldNum++;
        }
        if (addedMethodCallPossibleEntry.getType() != null) {
            typeNum++;
        }
        if (addedMethodCallPossibleEntry.getValue() != null) {
            valueNum++;
        }
    }

    public boolean hasStaticFieldClassAndFieldName() {
        return staticFieldClassAndFieldNameNum > 0;
    }

    public boolean hasNonStaticField() {
        return nonStaticFieldNum > 0;
    }

    public boolean hasType() {
        return typeNum > 0;
    }

    public boolean hasValue() {
        return valueNum > 0;
    }

    public boolean hasStaticFieldMethodCall() {
        return staticFieldMethodCallNum > 0;
    }

    public List<MethodCallPossibleEntry> getMethodCallPossibleEntryList() {
        return methodCallPossibleEntryList;
    }

    public boolean isArrayElement() {
        return arrayElement;
    }
}
