package com.adrninistrator.javacg.dto.field;

import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/1/4
 * @description: 字段所有可能的类型
 */
public class FieldPossibleTypes {
    /*
        记录字段所有可能的类型
        key
            字段名称
        value
            字段所有可能的类型列表
     */
    private final Map<String, List<String>> possibleTypeMap = new HashMap<>();

    /**
     * 为字段添加可能的类型
     *
     * @param fieldName
     * @param possibleType
     */
    public void addPossibleType(String fieldName, String possibleType) {
        if (JavaCGByteCodeUtil.isNullType(possibleType)) {
            return;
        }

        List<String> possibleTypeList = possibleTypeMap.computeIfAbsent(fieldName, k -> new ArrayList<>());
        if (!possibleTypeList.contains(possibleType)) {
            possibleTypeList.add(possibleType);
        }
    }

    /**
     * 获取字段可能的类型
     *
     * @param fieldName
     * @return
     */
    public List<String> getPossibleTypeList(String fieldName) {
        return possibleTypeMap.get(fieldName);
    }
}
