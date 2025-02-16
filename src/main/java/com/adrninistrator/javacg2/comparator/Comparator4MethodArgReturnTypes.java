package com.adrninistrator.javacg2.comparator;

import com.adrninistrator.javacg2.dto.method.MethodArgReturnTypes;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2022/11/26
 * @description:
 */
public class Comparator4MethodArgReturnTypes implements Comparator<MethodArgReturnTypes> {
    private static final Comparator4MethodArgReturnTypes INSTANCE = new Comparator4MethodArgReturnTypes();

    public static Comparator4MethodArgReturnTypes getInstance() {
        return INSTANCE;
    }

    private Comparator4MethodArgReturnTypes() {
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
