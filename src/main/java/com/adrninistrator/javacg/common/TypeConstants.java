package com.adrninistrator.javacg.common;

import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import org.apache.bcel.Const;

/**
 * @author adrninistrator
 * @date 2022/5/14
 * @description:
 */
public class TypeConstants {
    // 与int兼容的类型
    public static final String[] COMPATIBLE_INT_TYPES = new String[]{
            JavaCGConstantTypeEnum.CONSTTE_BOOLEAN.getType(),
            JavaCGConstantTypeEnum.CONSTTE_BYTE.getType(),
            JavaCGConstantTypeEnum.CONSTTE_SHORT.getType(),
            JavaCGConstantTypeEnum.CONSTTE_CHAR.getType(),
    };

    public static final String BYTE_ARRAY_TYPE = JavaCGConstantTypeEnum.CONSTTE_BYTE.getType() + JavaCGConstants.FLAG_ARRAY;
    public static final String BOOLEAN_ARRAY_TYPE = JavaCGConstantTypeEnum.CONSTTE_BOOLEAN.getType() + JavaCGConstants.FLAG_ARRAY;

    public static final short[] GOTO_OPCODES = new short[]{Const.GOTO, Const.GOTO_W};

    private TypeConstants() {
        throw new IllegalStateException("illegal");
    }
}
