package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import org.apache.bcel.generic.InstructionHandle;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description: JSR指令向操作数栈添加的元素
 */
public class JSRElement extends LocalVariableElement {

    // 使用value保存JSR下一条指令
    public JSRElement(InstructionHandle nextIh) {
        super(JavaCG2Constants.JSR_TYPE, false, nextIh, JavaCG2Constants.LOCAL_VARIABLE_INDEX_NOT_USED, null);
    }
}
