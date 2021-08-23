package com.adrninistrator.javacg.extension;

import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;

import java.util.List;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description:
 */
public interface CustomHandlerInterface {

    /**
     * 初始化
     */
    void init();

    /**
     * 开始处理一个jar包
     *
     * @param jarFilePath
     */
    void handleJar(String jarFilePath);

    /**
     * 对一个Class进行预处理
     *
     * @param javaClass
     */
    void preHandleClass(JavaClass javaClass);

    /**
     * 对一个Class进行处理
     *
     * @param javaClass
     */
    void handleClass(JavaClass javaClass);

    /**
     * 对一个方法调用进行预处理
     *
     * @param callId
     * @param calleeClassName
     * @param calleeMethodName
     * @param arguments
     * @param mcIh
     * @param methodGen
     */
    void handleMethodCall(int callId, String calleeClassName, String calleeMethodName, Type[] arguments, InstructionHandle mcIh, MethodGen methodGen);

    /**
     * 返回已获得的自定义数据
     *
     * @return
     */
    List<CustomData> getCustomDataList();

    /**
     * 返回当前的自定义数据类型
     *
     * @return
     */
    String getDataType();
}
