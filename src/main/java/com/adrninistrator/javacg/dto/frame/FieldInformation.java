package com.adrninistrator.javacg.dto.frame;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.element.variable.FieldElement;
import com.adrninistrator.javacg.util.JavaCGElementUtil;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/10/31
 * @description: 字段信息Map
 */
public class FieldInformation {

    /*
        key
            字段名称
        value
            字段信息
     */
    private Map<String, FieldElement> map;

    public FieldInformation() {
        map = new HashMap<>();
    }

    public void put(String fieldName, FieldElement fieldElement) {
        map.put(fieldName, fieldElement);
    }

    public void putStatic(String className, String fieldName, FieldElement fieldElement) {
        map.put(getStaticKey(className, fieldName), fieldElement);
    }

    public FieldElement get(String fieldName) {
        return map.get(fieldName);
    }

    public FieldElement getStatic(String className, String fieldName) {
        return map.get(getStaticKey(className, fieldName));
    }

    private String getStaticKey(String className, String fieldName) {
        return className + JavaCGConstants.FLAG_COLON + fieldName;
    }

    public FieldInformation copy() {
        FieldInformation clone = new FieldInformation();
        clone.map = new HashMap<>(map);
        return clone;
    }

    public int size() {
        return map.size();
    }

    /**
     * 比较本地变量，宽松模式
     *
     * @param existed
     * @param added
     * @param sameFieldInfoNameSet
     */
    public static void compareLooseMode(FieldInformation existed, FieldInformation added, Set<String> sameFieldInfoNameSet) {
        for (Map.Entry<String, FieldElement> entry : added.map.entrySet()) {
            String addedFieldName = entry.getKey();
            FieldElement addedFieldElement = entry.getValue();
            FieldElement existedFieldElement = existed.get(addedFieldName);

            if (existedFieldElement != null && JavaCGElementUtil.compare(existedFieldElement, addedFieldElement)) {
                sameFieldInfoNameSet.add(addedFieldName);
            }
        }
    }

    @Override
    public String toString() {
        return "num: " + map.size();
    }
}
