package com.adrninistrator.javacg.dto.frame;

import com.adrninistrator.javacg.dto.element.BaseElement;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGElementUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 操作数栈
 */
public class JavaCGOperandStack {

    private static final Logger logger = LoggerFactory.getLogger(JavaCGOperandStack.class);

    private final BaseElement[] elements;

    private int head;

    private final int capacity;

    public JavaCGOperandStack(int capacity) {
        head = 0;
        elements = new BaseElement[capacity];
        this.capacity = capacity;
    }

    public int size() {
        return head;
    }

    public boolean isEmpty() {
        return head == 0;
    }

    public void push(BaseElement element) {
        if (element == null) {
            logger.error("push的元素为null {}", head);
            throw new JavaCGRuntimeException("push的元素为null");
        }

        if (head >= capacity) {
            logger.error("栈已满 {} {}", head, element);
            throw new JavaCGRuntimeException("栈已满");
        }

        elements[head++] = element;

        logger.debug("入操作数栈后 size: {} {}", head, element);
    }

    public BaseElement pop() {
        if (head == 0) {
            logger.error("栈为空，不支持pop");
            throw new JavaCGRuntimeException("栈为空，不支持pop");
        }

        BaseElement element = elements[--head];

        logger.debug("出操作数栈后 size: {} {}", head, element);

        elements[head] = null;
        return element;
    }

    public BaseElement peek() {
        if (head == 0) {
            logger.error("栈为空，不支持peek");
            throw new JavaCGRuntimeException("栈为空，不支持peek");
        }

        return elements[head - 1];
    }

    /**
     * 从栈顶开始获取元素
     *
     * @param interval 距离栈顶的位置
     * @return
     */
    public BaseElement getFromTop(int interval) {
        int index = head - 1 - interval;
        if (index < 0) {
            return null;
        }

        return elements[index];
    }

    public void clear() {
        for (int i = 0; i < head; i++) {
            elements[i] = null;
        }
        head = 0;
    }

    public JavaCGOperandStack copy() {
        JavaCGOperandStack javaCGOperandStackCopy = new JavaCGOperandStack(capacity);
        for (int i = 0; i < this.head; i++) {
            javaCGOperandStackCopy.elements[i] = this.elements[i].copyElement();
        }
        javaCGOperandStackCopy.head = this.head;
        return javaCGOperandStackCopy;
    }

    /**
     * 比较当前对象与另一个对象值是否相同，宽松模式
     *
     * @param existed
     * @param added
     * @param sameStackSeqSet
     */
    public static void compareLooseMode(JavaCGOperandStack existed, JavaCGOperandStack added, Set<Integer> sameStackSeqSet) {
        // 比较栈长度
        if (existed.head != added.head) {
            return;
        }

        // 比较栈中的每一个元素
        for (int i = 0; i < existed.head; i++) {
            if (added.elements[i] == null ||
                    (existed.elements[i] != null && JavaCGElementUtil.compare(existed.elements[i], added.elements[i]))) {
                sameStackSeqSet.add(i);
            }
        }
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("head: ").append(head).append("\n");

        for (int i = 0; i < head; i++) {
            BaseElement baseElement = elements[i];
            stringBuilder.append(i).append(" ");
            if (baseElement != null) {
                stringBuilder.append(baseElement);
            } else {
                stringBuilder.append("null");
            }
            stringBuilder.append("\n");
        }

        return stringBuilder.toString();
    }
}
