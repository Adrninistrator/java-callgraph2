package com.adrninistrator.javacg.dto.call;

import com.adrninistrator.javacg.dto.field.FieldTypeAndName;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2023/3/12
 * @description: 方法调用中被调用对象、参数的可能的信息
 */
public class MethodCallPossibleEntry {

    // 可能的被调用静态字段
    private String staticFieldClassAndFieldName;

    // 可能的被调用非静态字段
    private FieldTypeAndName nonStaticField;

    // 可能的类型
    private String type;

    // 可能的值
    private Object value;

    // 被调用对象或参数是静态字段方法返回值的可能信息
    private String staticFieldMethodCall;

    // 记录被设置的内容数量
    private int contentNum = 0;

    public String getStaticFieldClassAndFieldName() {
        return staticFieldClassAndFieldName;
    }

    public void setStaticFieldClassAndFieldName(String staticFieldClassAndFieldName) {
        this.staticFieldClassAndFieldName = staticFieldClassAndFieldName;
        contentNum++;
    }

    /**
     * 比较与另一个对象值是否相同
     *
     * @param another
     * @return
     */
    public boolean compare(MethodCallPossibleEntry another) {
        if (!StringUtils.equals(staticFieldClassAndFieldName, another.staticFieldClassAndFieldName)) {
            return false;
        }

        if (!Objects.equals(nonStaticField, another.nonStaticField)) {
            return false;
        }

        if (!StringUtils.equals(type, another.type)) {
            return false;
        }

        if (!Objects.equals(value, another.value)) {
            return false;
        }

        if (!StringUtils.equals(staticFieldMethodCall, another.staticFieldMethodCall)) {
            return false;
        }
        return true;
    }

    public boolean hasContent() {
        return contentNum > 0;
    }

    public FieldTypeAndName getNonStaticField() {
        return nonStaticField;
    }

    public void setNonStaticField(FieldTypeAndName nonStaticField) {
        this.nonStaticField = nonStaticField;
        contentNum++;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
        contentNum++;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
        contentNum++;
    }

    public String getStaticFieldMethodCall() {
        return staticFieldMethodCall;
    }

    public void setStaticFieldMethodCall(String staticFieldMethodCall) {
        this.staticFieldMethodCall = staticFieldMethodCall;
        contentNum++;
    }
}
