package com.adrninistrator.javacg.common;

import com.adrninistrator.javacg.enums.ConstantTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/5/14
 * @description:
 */
public class TypeConstants {
    // 与int兼容的类型
    public static final String[] COMPATIBLE_INT_TYPES = new String[]{
            ConstantTypeEnum.CONSTTE_BOOLEAN.getType(),
            ConstantTypeEnum.CONSTTE_BYTE.getType(),
            ConstantTypeEnum.CONSTTE_SHORT.getType(),
            ConstantTypeEnum.CONSTTE_CHAR.getType(),
    };

    public static final String BYTE_ARRAY_TYPE = ConstantTypeEnum.CONSTTE_BYTE.getType() + JavaCGConstants.FLAG_ARRAY;
    public static final String BOOLEAN_ARRAY_TYPE = ConstantTypeEnum.CONSTTE_BOOLEAN.getType() + JavaCGConstants.FLAG_ARRAY;

    private TypeConstants() {
        throw new IllegalStateException("illegal");
    }
}
