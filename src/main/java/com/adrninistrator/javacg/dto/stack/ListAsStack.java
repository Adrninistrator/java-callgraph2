package com.adrninistrator.javacg.dto.stack;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/1
 * @description: 當作棧使用的List
 */
public class ListAsStack<E> {
    private int head = -1;

    private final List<E> list = new ArrayList<>();

    /**
     * 出棧
     *
     * @return
     */
    public E pop() {
        E element = list.get(head);
        head--;
        return element;
    }

    /**
     * 删除栈顶元素
     */
    public void removeTop() {
        head--;
    }

    /**
     * 獲取棧頂元素
     *
     * @return
     */
    public E peek() {
        return list.get(head);
    }

    /**
     * 入棧
     *
     * @param element
     */
    public void push(E element) {
        head++;
        if (head >= list.size()) {
            // head对应的记录不存在，则添加
            list.add(element);
        } else {
            // head对应的记录已存在，则设置
            list.set(head, element);
        }
    }

    /**
     * 获取指定下标的元素
     *
     * @param index
     * @return
     */
    public E getElement(int index) {
        return list.get(index);
    }

    /**
     * 判断栈是否为空
     *
     * @return
     */
    public boolean isEmpty() {
        return head < 0;
    }

    /**
     * 判断栈是否处在最下面的位置（有一个元素）
     *
     * @return
     */
    public boolean atBottom() {
        return head == 0;
    }

    /**
     * 获取头的位置
     *
     * @return
     */
    public int getHead() {
        return head;
    }

    /**
     * 获取指定下标的元素
     *
     * @param index
     * @return
     */
    public E getAt(int index) {
        return list.get(index);
    }
}
