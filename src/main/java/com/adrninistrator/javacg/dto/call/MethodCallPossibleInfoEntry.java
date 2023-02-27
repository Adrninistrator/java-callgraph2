package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg.dto.element.variable.StaticFieldMethodCallElement;
import com.adrninistrator.javacg.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/11/3
 * @description: 方法调用中被调用对象、参数的可能的信息
 */
public class MethodCallPossibleInfoEntry {

    // 可能的被调用静态字段
    private List<String> possibleStaticFieldClassAndFieldNameList;

    // 可能的被调用非静态字段
    private List<FieldTypeAndName> possibleNonStaticFieldList;

    // 可能的类型
    private List<String> possibleTypeList;

    // 可能的值
    private List<Object> possibleValueList;

    // 被调用对象或参数是静态字段方法返回值的可能信息
    private List<String> possibleStaticFieldMethodCallList;

    /**
     * 添加可能的信息
     *
     * @param baseElement    操作数栈中的元素
     * @param typeInByteCode 字节码中的类型
     */
    public void addPossibleInfo(BaseElement baseElement, String typeInByteCode) {
        if (baseElement == null) {
            return;
        }

        if (baseElement instanceof StaticFieldMethodCallElement) {
            // 添加被调用对象或参数是静态字段方法返回值的可能信息
            addStaticFieldMethodCallElement((StaticFieldMethodCallElement) baseElement);
        } else if (baseElement instanceof StaticFieldElement) {
            // 添加可能的被调用静态字段
            addPossibleStaticField((StaticFieldElement) baseElement);
        } else if (baseElement instanceof FieldElement) {
            // 添加可能的被调用非静态字段
            addPossibleNonStaticField((FieldElement) baseElement);
        }

        String type = baseElement.getType();
        if (!JavaCGByteCodeUtil.isNullType(type) &&
                !type.equals(typeInByteCode) &&
                !JavaCGByteCodeUtil.compareIntType(type, typeInByteCode) &&
                !JavaCGByteCodeUtil.compareByteBooleanType(type, typeInByteCode)
        ) {
            /*
                添加可能的类型
                若操作数栈中元素的类型与字节码中的类型相同，则不添加
                若为int及兼容的类型，则不添加
                若为兼容的byte、boolean类型，则不添加
             */
            addPossibleType(type);
        }

        Object value = baseElement.getValue();
        if (value != null) {
            // 添加可能的值
            addPossibleValue(value);
        }
    }

    // 添加可能的被调用静态字段
    private void addPossibleStaticField(StaticFieldElement addedStaticFieldElement) {
        String addedStaticFieldClassAndFieldName = addedStaticFieldElement.getClassAndFieldName();
        if (possibleStaticFieldClassAndFieldNameList == null) {
            possibleStaticFieldClassAndFieldNameList = new ArrayList<>();
            possibleStaticFieldClassAndFieldNameList.add(addedStaticFieldClassAndFieldName);
            return;
        }

        if (!possibleStaticFieldClassAndFieldNameList.contains(addedStaticFieldClassAndFieldName)) {
            possibleStaticFieldClassAndFieldNameList.add(addedStaticFieldClassAndFieldName);
        }
    }

    // 添加可能的被调用非静态字段
    private void addPossibleNonStaticField(FieldElement addedFieldElement) {
        FieldTypeAndName addedFieldTypeAndName = new FieldTypeAndName(addedFieldElement.getType(), addedFieldElement.getFieldName());
        if (possibleNonStaticFieldList == null) {
            possibleNonStaticFieldList = new ArrayList<>();
            possibleNonStaticFieldList.add(addedFieldTypeAndName);
            return;
        }

        for (FieldTypeAndName existedFieldTypeAndName : possibleNonStaticFieldList) {
            if (addedFieldTypeAndName.compare(existedFieldTypeAndName)) {
                return;
            }
        }

        possibleNonStaticFieldList.add(addedFieldTypeAndName);
    }

    // 添加可能的类型
    private void addPossibleType(String addedType) {
        if (possibleTypeList == null) {
            possibleTypeList = new ArrayList<>();
            possibleTypeList.add(addedType);
            return;
        }

        if (!possibleTypeList.contains(addedType)) {
            possibleTypeList.add(addedType);
        }
    }

    // 添加可能的值
    private void addPossibleValue(Object addedValue) {
        if (possibleValueList == null) {
            possibleValueList = new ArrayList<>();
            possibleValueList.add(addedValue);
            return;
        }

        if (!possibleValueList.contains(addedValue)) {
            possibleValueList.add(addedValue);
        }
    }

    // 添加被调用对象或参数是静态字段方法返回值的可能信息
    private void addStaticFieldMethodCallElement(StaticFieldMethodCallElement staticFieldMethodCallElement) {
        String staticFieldMethodCallInfo = staticFieldMethodCallElement.getInfo();
        if (possibleStaticFieldMethodCallList == null) {
            possibleStaticFieldMethodCallList = new ArrayList<>();
            possibleStaticFieldMethodCallList.add(staticFieldMethodCallInfo);
            return;
        }

        if (!possibleStaticFieldMethodCallList.contains(staticFieldMethodCallInfo)) {
            possibleStaticFieldMethodCallList.add(staticFieldMethodCallInfo);
        }
    }

    //
    public List<String> getPossibleStaticFieldClassAndFieldNameList() {
        return possibleStaticFieldClassAndFieldNameList;
    }

    public List<FieldTypeAndName> getPossibleNonStaticFieldList() {
        return possibleNonStaticFieldList;
    }

    public List<String> getPossibleTypeList() {
        return possibleTypeList;
    }

    public List<Object> getPossibleValueList() {
        return possibleValueList;
    }

    public List<String> getPossibleStaticFieldMethodCallList() {
        return possibleStaticFieldMethodCallList;
    }
}
