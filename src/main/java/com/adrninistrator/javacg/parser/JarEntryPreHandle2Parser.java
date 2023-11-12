package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.dto.interfaces.InterfaceExtendsMethodInfo;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg.spring.UseSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarInputStream;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，第二次预处理
 */
public class JarEntryPreHandle2Parser extends AbstractJarEntryParser {

    private Set<String> classExtendsSet;

    private Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap;

    private Map<String, List<String>> childrenClassMap;

    private Set<String> interfaceExtendsSet;

    private Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap;

    private Map<String, List<String>> childrenInterfaceMap;

    private final UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler;

    public JarEntryPreHandle2Parser(JavaCGConfInfo javaCGConfInfo, Map<String, JarInfo> jarInfoMap, UseSpringBeanByAnnotationHandler useSpringBeanByAnnotationHandler) {
        super(javaCGConfInfo, jarInfoMap);
        this.useSpringBeanByAnnotationHandler = useSpringBeanByAnnotationHandler;
    }

    @Override
    protected boolean handleEntry(JarInputStream jarInputStream, String jarEntryName) throws IOException {
        // 尝试处理jar包中的class文件
        tryHandleClassEntry(jarInputStream, jarEntryName);
        // 以上方法返回值不处理
        return true;
    }

    @Override
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryName) {
        if (javaClass.isClass()) {
            // 处理类
            // 查找涉及继承的类的信息，需要提前执行，使后续处理方法调用时，classExtendsMethodInfoMap的数据是完整的
            findClassExtendsInfo(javaClass);

            if (javaCGConfInfo.isParseMethodCallTypeValue()) {
                // 记录类中带有Spring相关注解的字段信息
                useSpringBeanByAnnotationHandler.recordClassFieldsWithSpringAnnotation(javaClass);
            }
            return true;
        }

        // 处理接口
        findInterfaceExtendsInfo(javaClass);
        return true;
    }

    // 查找涉及继承的类的信息
    private void findClassExtendsInfo(JavaClass javaClass) {
        String className = javaClass.getClassName();
        if (!classExtendsSet.contains(className) || classExtendsMethodInfoMap.get(className) != null) {
            // 假如当前类不涉及继承，或当前类已处理过，则不处理
            return;
        }

        String superClassName = javaClass.getSuperclassName();
        if (!JavaCGUtil.isClassInJdk(superClassName)) {
            // 记录父类及其子类，忽略以"java."开头的父类
            List<String> childrenClassList = childrenClassMap.computeIfAbsent(superClassName, k -> new ArrayList<>());
            childrenClassList.add(className);
        }

        Map<MethodArgReturnTypes, Integer> methodAttributeMap = new HashMap<>();
        // 遍历类的方法
        for (Method method : javaClass.getMethods()) {
            String methodName = method.getName();
            if (JavaCGByteCodeUtil.checkExtendsMethod(methodName, method)) {
                // 对于可能涉及继承的方法进行记录
                MethodArgReturnTypes methodArgReturnTypes = new MethodArgReturnTypes(methodName, method.getArgumentTypes(), method.getReturnType());
                methodAttributeMap.put(methodArgReturnTypes, method.getAccessFlags());
            }
        }
        classExtendsMethodInfoMap.put(className, new ClassExtendsMethodInfo(javaClass.getAccessFlags(), superClassName, methodAttributeMap));
    }

    // 查找涉及继承的接口的信息
    private void findInterfaceExtendsInfo(JavaClass interfaceClass) {
        String interfaceName = interfaceClass.getClassName();
        if (interfaceClass.isAnnotation() ||
                !interfaceExtendsSet.contains(interfaceName) ||
                interfaceExtendsMethodInfoMap.get(interfaceName) != null) {
            // 假如为水底有，或当前接口不涉及继承，或当前接口已处理过，则不处理
            return;
        }

        String[] superInterfaceNames = interfaceClass.getInterfaceNames();
        for (String superInterfaceName : superInterfaceNames) {
            // 记录父类及其子类，忽略以"java."开头的父类
            List<String> childrenInterfaceList = childrenInterfaceMap.computeIfAbsent(superInterfaceName, k -> new ArrayList<>());
            childrenInterfaceList.add(interfaceName);
        }

        // 记录当前接口的方法信息
        List<MethodArgReturnTypes> methodAttributeList = new ArrayList<>();
        for (Method method : interfaceClass.getMethods()) {
            methodAttributeList.add(new MethodArgReturnTypes(method.getName(), method.getArgumentTypes(), method.getReturnType()));
        }
        interfaceExtendsMethodInfoMap.put(interfaceName, new InterfaceExtendsMethodInfo(Arrays.asList(superInterfaceNames), methodAttributeList));
    }

    //
    public void setClassExtendsSet(Set<String> classExtendsSet) {
        this.classExtendsSet = classExtendsSet;
    }

    public void setClassExtendsMethodInfoMap(Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap) {
        this.classExtendsMethodInfoMap = classExtendsMethodInfoMap;
    }

    public void setChildrenClassMap(Map<String, List<String>> childrenClassMap) {
        this.childrenClassMap = childrenClassMap;
    }

    public void setInterfaceExtendsSet(Set<String> interfaceExtendsSet) {
        this.interfaceExtendsSet = interfaceExtendsSet;
    }

    public void setInterfaceExtendsMethodInfoMap(Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap) {
        this.interfaceExtendsMethodInfoMap = interfaceExtendsMethodInfoMap;
    }

    public void setChildrenInterfaceMap(Map<String, List<String>> childrenInterfaceMap) {
        this.childrenInterfaceMap = childrenInterfaceMap;
    }
}
