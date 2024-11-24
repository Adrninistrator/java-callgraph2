package com.adrninistrator.javacg2.dto.frame;

import com.adrninistrator.javacg2.dto.element.variable.FieldElement;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2ElementUtil;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/10/31
 * @description: 字段信息Map
 */
public class FieldInformationMap {

    /*
        key
            字段名称
        value
            字段信息
     */
    private Map<String, FieldElement> map;

    public FieldInformationMap() {
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
        return JavaCG2ClassMethodUtil.genClassAndField(className, fieldName);
    }

    public FieldInformationMap copy() {
        FieldInformationMap fieldInformationMapCopy = new FieldInformationMap();
        fieldInformationMapCopy.map = new HashMap<>(this.map.size());
        for (Map.Entry<String, FieldElement> entry : this.map.entrySet()) {
            fieldInformationMapCopy.map.put(entry.getKey(), (FieldElement) entry.getValue().copyElement());
        }
        return fieldInformationMapCopy;
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
    public static void compareLooseMode(FieldInformationMap existed, FieldInformationMap added, Set<String> sameFieldInfoNameSet) {
        for (Map.Entry<String, FieldElement> entry : added.map.entrySet()) {
            String addedFieldName = entry.getKey();
            FieldElement addedFieldElement = entry.getValue();
            FieldElement existedFieldElement = existed.get(addedFieldName);

            if (existedFieldElement != null && JavaCG2ElementUtil.compare(existedFieldElement, addedFieldElement)) {
                sameFieldInfoNameSet.add(addedFieldName);
            }
        }
    }

    @Override
    public String toString() {
        return "num: " + map.size();
    }
}
