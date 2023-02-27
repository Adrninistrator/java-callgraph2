package com.adrninistrator.javacg.comparator;

import com.adrninistrator.javacg.dto.method.MethodAndArgs;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2022/11/26
 * @description:
 */
public class MethodAndArgsComparator implements Comparator<MethodAndArgs> {
    private static final MethodAndArgsComparator INSTANCE = new MethodAndArgsComparator();

    public static MethodAndArgsComparator getInstance() {
        return INSTANCE;
    }

    private MethodAndArgsComparator() {
    }

    @Override
    public int compare(MethodAndArgs o1, MethodAndArgs o2) {
        int result1 = o1.getMethodName().compareTo(o2.getMethodName());
        if (result1 != 0) {
            return result1;
        }

        return o1.getMethodArgs().compareTo(o2.getMethodArgs());
    }
}
