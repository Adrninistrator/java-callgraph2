package com.adrninistrator.javacg.dto.jar;

import com.adrninistrator.javacg.common.JavaCGConstants;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/4/18
 * @description: 类名及对应的Jar包序号
 */
public class ClassAndJarNum {

    private final Map<String, Integer> classAndJarNumMap = new HashMap<>(JavaCGConstants.SIZE_100);

    /**
     * 根据类名获取对应的Jar包序号，若不存在则返回代表为空的Jar包序号
     *
     * @param className
     * @return
     */
    public String getJarNum(String className) {
        Integer classJarNum = classAndJarNumMap.get(className);
        return classJarNum == null ? JavaCGConstants.EMPTY_JAR_NUM : String.valueOf(classJarNum);
    }

    /**
     * 记录类名及对应的Jar包序号
     *
     * @param className
     * @param jarNum
     */
    public void put(String className, Integer jarNum) {
        // 只记录每个类所出现的最靠前的Jar包序号，不覆盖现有值
        classAndJarNumMap.putIfAbsent(className, jarNum);
    }
}
