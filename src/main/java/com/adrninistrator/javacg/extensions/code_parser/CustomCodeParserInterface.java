package com.adrninistrator.javacg.extensions.code_parser;

import com.adrninistrator.javacg.dto.classes.ClassInterfaceMethodInfo;
import com.adrninistrator.javacg.dto.classes.ExtendsClassMethodInfo;
import com.adrninistrator.javacg.dto.counter.CallIdCounter;
import com.adrninistrator.javacg.dto.method.MethodCallDto;
import com.adrninistrator.javacg.extensions.dto.ExtendedData;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;

import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description:
 */
public interface CustomCodeParserInterface {

    /**
     * 初始化，整个执行过程中只执行一次
     */
    default void init() {
    }

    /**
     * 开始处理一个jar包
     *
     * @param jarFilePath
     */
    default void handleJar(String jarFilePath) {
    }

    /**
     * 处理一个jar包中的文件
     *
     * @param jarFile
     * @param jarEntry
     */
    default void handleJarEntryFile(JarFile jarFile, JarEntry jarEntry) {
    }

    /**
     * 对一个Class进行预处理
     *
     * @param javaClass
     */
    default void preHandleClass(JavaClass javaClass) {
    }

    /**
     * 对一个Class进行处理
     *
     * @param javaClass
     */
    default void handleClass(JavaClass javaClass) {
    }

    /**
     * 对一个方法调用进行预处理
     *
     * @param callIdCounter
     * @param calleeClassName
     * @param calleeMethodName
     * @param arguments
     * @param mcIh
     * @param methodGen
     * @param methodCalls
     */
    default void handleMethodCall(CallIdCounter callIdCounter, String calleeClassName, String calleeMethodName, Type[] arguments, InstructionHandle mcIh, MethodGen methodGen,
                                  List<MethodCallDto> methodCalls) {
    }

    /**
     * 返回已获得的自定义数据，整个执行过程中只执行一次
     *
     * @return
     */
    default List<ExtendedData> getExtendedDataList() {
        return null;
    }

    /**
     * 操作完成之前的处理，整个执行过程中只执行一次
     */
    default void beforeDone() {
    }

    /**
     * 返回当前的自定义数据类型
     *
     * @return
     */
    default String getDataType() {
        return null;
    }

    /**
     * 接收保存涉及继承类的Map信息
     *
     * @param extendsClassMethodInfoMap
     */
    default void setExtendsClassMethodInfoMap(Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap) {
    }

    /**
     * 接收保存类实现接口的Map信息
     *
     * @param classInterfaceMethodInfoMap
     */
    default void setClassInterfaceMethodInfoMap(Map<String, ClassInterfaceMethodInfo> classInterfaceMethodInfoMap) {
    }

    /**
     * 接收保存接口之间继承关系的Map信息
     *
     * @param interfaceExtendsMap
     */
    default void setInterfaceExtendsMap(Map<String, String> interfaceExtendsMap) {
    }

    /**
     * 指定不需要补充子类调用父类方法/父类调用子类方法的顶层父类完整类名
     *
     * @return
     */
    default String chooseSkipTopSuperClassFullName() {
        return null;
    }
}
