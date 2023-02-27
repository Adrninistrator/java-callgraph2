package com.adrninistrator.javacg.dto.element.variable;

import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.bcel.generic.InstructionHandle;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description: JSR指令向操作数栈添加的元素
 */
public class JSRElement extends LocalVariableElement {

    // 使用value保存JSR下一条指令
    public JSRElement(InstructionHandle nextIh) {
        super(JavaCGConstants.JSR_TYPE, nextIh, JavaCGConstants.LOCAL_VARIABLE_INDEX_NOT_USED);
    }
}
