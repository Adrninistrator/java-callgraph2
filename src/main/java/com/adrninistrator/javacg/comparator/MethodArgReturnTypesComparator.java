package com.adrninistrator.javacg.comparator;

import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2022/11/26
 * @description:
 */
public class MethodArgReturnTypesComparator implements Comparator<MethodArgReturnTypes> {
    private static final MethodArgReturnTypesComparator INSTANCE = new MethodArgReturnTypesComparator();

    public static MethodArgReturnTypesComparator getInstance() {
        return INSTANCE;
    }

    private MethodArgReturnTypesComparator() {
    }

    @Override
    public int compare(MethodArgReturnTypes o1, MethodArgReturnTypes o2) {
        int result1 = o1.getMethodName().compareTo(o2.getMethodName());
        if (result1 != 0) {
            return result1;
        }

        return o1.getMethodArgTypes().compareTo(o2.getMethodArgTypes());
    }
}
