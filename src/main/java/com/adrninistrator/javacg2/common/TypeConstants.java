package com.adrninistrator.javacg2.common;

import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import org.apache.bcel.Const;

/**
 * @author adrninistrator
 * @date 2022/5/14
 * @description:
 */
public class TypeConstants {
    // 与int兼容的类型
    public static final String[] COMPATIBLE_INT_TYPES = new String[]{
            JavaCG2ConstantTypeEnum.CONSTTE_BOOLEAN.getType(),
            JavaCG2ConstantTypeEnum.CONSTTE_BYTE.getType(),
            JavaCG2ConstantTypeEnum.CONSTTE_SHORT.getType(),
            JavaCG2ConstantTypeEnum.CONSTTE_CHAR.getType(),
    };

    public static final String BYTE_ARRAY_TYPE = JavaCG2ConstantTypeEnum.CONSTTE_BYTE.getType() + JavaCG2Constants.FLAG_ARRAY;
    public static final String BOOLEAN_ARRAY_TYPE = JavaCG2ConstantTypeEnum.CONSTTE_BOOLEAN.getType() + JavaCG2Constants.FLAG_ARRAY;

    public static final short[] GOTO_OPCODES = new short[]{Const.GOTO, Const.GOTO_W};

    private TypeConstants() {
        throw new IllegalStateException("illegal");
    }
}
