package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.comparator.MethodAndArgsComparator;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.call.MethodCall;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.classes.Node4ClassExtendsMethod;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.interfaces.InterfaceExtendsMethodInfo;
import com.adrninistrator.javacg.dto.method.MethodAndArgs;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGLogUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/13
 * @description: 继承及实现相关的方法处理类
 */
public class ExtendsImplHandler {
    private JavaCGConfInfo javaCGConfInfo;

    private JavaCGCounter callIdCounter;

    private Map<String, List<MethodAndArgs>> interfaceMethodWithArgsMap;

    private Map<String, List<String>> childrenClassMap;

    private Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap;

    private Map<String, List<String>> childrenInterfaceMap;

    private Map<String, ClassImplementsMethodInfo> classInterfaceMethodInfoMap;

    private Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap;

    public void handle(Writer methodCallWriter) throws IOException {
        // 将父接口中的方法添加到子接口中
        addSuperInterfaceMethod4ChildrenInterface(methodCallWriter);

        // 将接口中的抽象方法添加到抽象父类中
        addInterfaceMethod4SuperAbstractClass();

        // 记录接口调用实现类方法
        recordInterfaceCallClassMethod(methodCallWriter);

        // 记录父类调用子类方法，及子类调用父类方法
        recordClassExtendsMethod(methodCallWriter);
    }

    // 将父接口中的方法添加到子接口中
    private void addSuperInterfaceMethod4ChildrenInterface(Writer methodCallWriter) throws IOException {
        // 查找顶层父接口
        Set<String> topSuperInterfaceSet = new HashSet<>();
        for (Map.Entry<String, InterfaceExtendsMethodInfo> entry : interfaceExtendsMethodInfoMap.entrySet()) {
            InterfaceExtendsMethodInfo interfaceExtendsMethodInfo = entry.getValue();
            for (String superInterface : interfaceExtendsMethodInfo.getSuperInterfaceList()) {
                InterfaceExtendsMethodInfo superInterfaceExtendsMethodInfo = interfaceExtendsMethodInfoMap.get(superInterface);
                if (superInterfaceExtendsMethodInfo == null || superInterfaceExtendsMethodInfo.getSuperInterfaceList().isEmpty()) {
                    // 父接口在接口涉及继承的信息Map中不存在记录，或父接口列表为空，说明当前为顶层父接口
                    if (!topSuperInterfaceSet.add(superInterface)) {
                        continue;
                    }
                    if (JavaCGLogUtil.isDebugPrintFlag()) {
                        JavaCGLogUtil.debugPrint("处理一个顶层父接口: " + superInterface);
                    }
                }
            }
        }

        List<String> topSuperInterfaceSetList = new ArrayList<>(topSuperInterfaceSet);
        // 对顶层父接口类名排序
        Collections.sort(topSuperInterfaceSetList);
        for (String topSuperInterface : topSuperInterfaceSetList) {
            // 遍历顶层父接口并处理
            handleOneSuperInterface(topSuperInterface, methodCallWriter);
        }
    }

    // 处理一个父接口
    private void handleOneSuperInterface(String superInterface, Writer methodCallWriter) throws IOException {
        List<String> childrenInterfaceList = childrenInterfaceMap.get(superInterface);
        if (childrenInterfaceList == null) {
            return;
        }

        for (String childrenInterface : childrenInterfaceList) {
            // 处理父接口及一个子接口
            handleSuperAndChildInterface(superInterface, childrenInterface, methodCallWriter);

            // 继续处理子接口
            handleOneSuperInterface(childrenInterface, methodCallWriter);
        }
    }

    // 处理父接口及一个子接口
    private void handleSuperAndChildInterface(String superInterface, String childInterface, Writer methodCallWriter) throws IOException {
        InterfaceExtendsMethodInfo superInterfaceExtendsMethodInfo = interfaceExtendsMethodInfoMap.get(superInterface);
        if (superInterfaceExtendsMethodInfo == null) {
            // 父接口在接口涉及继承的信息Map中不存在记录时，不处理
            return;
        }

        InterfaceExtendsMethodInfo childInterfaceExtendsMethodInfo = interfaceExtendsMethodInfoMap.get(childInterface);

        List<MethodAndArgs> superInterfaceMethodAndArgsList = superInterfaceExtendsMethodInfo.getMethodAndArgsList();
        // 对父接口中的方法进行排序
        superInterfaceMethodAndArgsList.sort(MethodAndArgsComparator.getInstance());
        // 遍历父接口中的方法
        for (MethodAndArgs superMethodAndArgs : superInterfaceMethodAndArgsList) {
            List<MethodAndArgs> childInterfaceMethodAndArgsList = childInterfaceExtendsMethodInfo.getMethodAndArgsList();
            if (childInterfaceMethodAndArgsList.contains(superMethodAndArgs)) {
                // 子接口中已包含父接口，跳过
                continue;
            }

            // 在子接口中添加父接口的方法（涉及继承的接口相关结构）
            childInterfaceMethodAndArgsList.add(superMethodAndArgs);

            // 在子接口中添加父接口的方法（所有接口都需要记录的结构）
            List<MethodAndArgs> childInterfaceMethodAndArgsListAll = interfaceMethodWithArgsMap.computeIfAbsent(childInterface, k -> new ArrayList<>());
            childInterfaceMethodAndArgsListAll.add(superMethodAndArgs);

            // 添加子接口调用父接口方法
            addExtraMethodCall(methodCallWriter, childInterface, superMethodAndArgs.getMethodName(), superMethodAndArgs.getMethodArgs(),
                    CallTypeEnum.CTE_CHILD_CALL_SUPER_INTERFACE, superInterface, superMethodAndArgs.getMethodName(), superMethodAndArgs.getMethodArgs());
        }
    }

    // 将接口中的抽象方法加到抽象父类中
    private void addInterfaceMethod4SuperAbstractClass() {
        for (Map.Entry<String, List<String>> childrenClassEntry : childrenClassMap.entrySet()) {
            String superClassName = childrenClassEntry.getKey();
            ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoMap.get(superClassName);
            if (classExtendsMethodInfo == null || !JavaCGByteCodeUtil.isAbstractFlag(classExtendsMethodInfo.getAccessFlags())) {
                /*
                    为空的情况，对应其他jar包中的Class可以找到，但是找不到它们的方法，是正常的，不处理
                    若不是抽象类则不处理
                 */
                continue;
            }

            ClassImplementsMethodInfo classImplementsMethodInfo = classInterfaceMethodInfoMap.get(superClassName);
            if (classImplementsMethodInfo == null) {
                continue;
            }

            Map<MethodAndArgs, Integer> methodWithArgsMap = classExtendsMethodInfo.getMethodWithArgsMap();

            int accessFlags = 0;
            accessFlags = JavaCGByteCodeUtil.setAbstractFlag(accessFlags, true);
            accessFlags = JavaCGByteCodeUtil.setPublicFlag(accessFlags, true);
            accessFlags = JavaCGByteCodeUtil.setProtectedFlag(accessFlags, false);

            // 将接口中的抽象方法加到抽象父类中
            for (String interfaceName : classImplementsMethodInfo.getInterfaceNameList()) {
                List<MethodAndArgs> interfaceMethodWithArgsList = interfaceMethodWithArgsMap.get(interfaceName);
                if (interfaceMethodWithArgsList == null) {
                    continue;
                }

                for (MethodAndArgs interfaceMethodWithArgs : interfaceMethodWithArgsList) {
                    methodWithArgsMap.putIfAbsent(interfaceMethodWithArgs, accessFlags);
                }
            }
        }
    }

    // 记录父类调用子类方法，及子类调用父类方法
    private void recordClassExtendsMethod(Writer methodCallWriter) throws IOException {
        Set<String> topSuperClassNameSet = new HashSet<>();

        // 得到最顶层父类名称
        for (Map.Entry<String, ClassExtendsMethodInfo> classExtendsMethodInfoEntry : classExtendsMethodInfoMap.entrySet()) {
            String className = classExtendsMethodInfoEntry.getKey();
            ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoEntry.getValue();
            String superClassName = classExtendsMethodInfo.getSuperClassName();
            if (JavaCGUtil.isClassInJdk(superClassName)) {
                topSuperClassNameSet.add(className);
            }
        }

        List<String> topSuperClassNameList = new ArrayList<>(topSuperClassNameSet);
        // 对顶层父类类名排序
        Collections.sort(topSuperClassNameList);
        for (String topSuperClassName : topSuperClassNameList) {
            // 处理一个顶层父类
            handleOneTopSuperClass(topSuperClassName, methodCallWriter);
        }
    }

    // 处理一个顶层父类
    private void handleOneTopSuperClass(String topSuperClassName, Writer methodCallWriter) throws IOException {
        if (JavaCGLogUtil.isDebugPrintFlag()) {
            JavaCGLogUtil.debugPrint("处理一个顶层父类: " + topSuperClassName);
        }
        ListAsStack<Node4ClassExtendsMethod> nodeStack = new ListAsStack<>();

        // 初始化节点列表
        Node4ClassExtendsMethod topNode = new Node4ClassExtendsMethod(topSuperClassName, JavaCGConstants.EXTENDS_NODE_INDEX_INIT);
        nodeStack.push(topNode);

        // 开始循环
        while (true) {
            Node4ClassExtendsMethod currentNode = nodeStack.peek();
            List<String> childrenClassList = childrenClassMap.get(currentNode.getSuperClassName());
            if (childrenClassList == null) {
                System.err.println("### 未找到顶层父类的子类: " + currentNode.getSuperClassName());
                return;
            }

            // 对子类类名排序
            Collections.sort(childrenClassList);
            int currentChildClassIndex = currentNode.getChildClassIndex() + 1;
            if (currentChildClassIndex >= childrenClassList.size()) {
                if (nodeStack.atBottom()) {
                    return;
                }
                // 删除栈顶元素
                nodeStack.removeTop();
                continue;
            }

            // 处理当前的子类
            String childClassName = childrenClassList.get(currentChildClassIndex);

            // 处理父类和子类的方法调用
            handleSuperAndChildClass(currentNode.getSuperClassName(), childClassName, methodCallWriter);

            // 处理下一个子类
            currentNode.setChildClassIndex(currentChildClassIndex);

            List<String> nextChildClassList = childrenClassMap.get(childClassName);
            if (nextChildClassList == null) {
                // 当前的子类下没有子类
                continue;
            }

            // 当前的子类下有子类
            // 入栈
            Node4ClassExtendsMethod nextNode = new Node4ClassExtendsMethod(childClassName, JavaCGConstants.EXTENDS_NODE_INDEX_INIT);
            nodeStack.push(nextNode);
        }
    }

    // 处理父类和子类的方法调用
    private void handleSuperAndChildClass(String superClassName, String childClassName, Writer methodCallWriter) throws IOException {
        ClassExtendsMethodInfo superClassMethodInfo = classExtendsMethodInfoMap.get(superClassName);
        if (superClassMethodInfo == null) {
            System.err.println("### 未找到父类信息: " + superClassName);
            return;
        }

        ClassExtendsMethodInfo childClassMethodInfo = classExtendsMethodInfoMap.get(childClassName);
        if (childClassMethodInfo == null) {
            System.err.println("### 未找到子类信息: " + childClassName);
            return;
        }

        Map<MethodAndArgs, Integer> superMethodWithArgsMap = superClassMethodInfo.getMethodWithArgsMap();
        Map<MethodAndArgs, Integer> childMethodWithArgsMap = childClassMethodInfo.getMethodWithArgsMap();

        List<MethodAndArgs> superMethodAndArgsList = new ArrayList<>(superMethodWithArgsMap.keySet());
        // 对父类方法进行排序
        superMethodAndArgsList.sort(MethodAndArgsComparator.getInstance());
        // 遍历父类方法
        for (MethodAndArgs superMethodWithArgs : superMethodAndArgsList) {
            Integer superMethodAccessFlags = superMethodWithArgsMap.get(superMethodWithArgs);
            if (JavaCGByteCodeUtil.isAbstractFlag(superMethodAccessFlags)) {
                // 处理父类抽象方法
                childMethodWithArgsMap.putIfAbsent(superMethodWithArgs, superMethodAccessFlags);
                // 添加父类调用子类的方法调用
                addExtraMethodCall(methodCallWriter, superClassName, superMethodWithArgs.getMethodName(), superMethodWithArgs.getMethodArgs(), CallTypeEnum.CTE_SUPER_CALL_CHILD,
                        childClassName, superMethodWithArgs.getMethodName(), superMethodWithArgs.getMethodArgs());
                continue;
            }

            if (JavaCGByteCodeUtil.isPublicFlag(superMethodAccessFlags) || JavaCGByteCodeUtil.isProtectedMethod(superMethodAccessFlags)) {
                // 父类的public/protected且非抽象方法
                if (childMethodWithArgsMap.get(superMethodWithArgs) != null) {
                    continue;
                }

                childMethodWithArgsMap.put(superMethodWithArgs, superMethodAccessFlags);

                // 添加子类调用父类方法
                addExtraMethodCall(methodCallWriter, childClassName, superMethodWithArgs.getMethodName(), superMethodWithArgs.getMethodArgs(), CallTypeEnum.CTE_CHILD_CALL_SUPER,
                        superClassName, superMethodWithArgs.getMethodName(), superMethodWithArgs.getMethodArgs());
            }
        }
    }

    // 记录接口调用实现类方法
    private void recordInterfaceCallClassMethod(Writer methodCallWriter) throws IOException {
        if (classInterfaceMethodInfoMap.isEmpty() || interfaceMethodWithArgsMap.isEmpty()) {
            return;
        }


        List<String> classNameList = new ArrayList<>(classInterfaceMethodInfoMap.keySet());
        // 对类名进行排序
        Collections.sort(classNameList);
        // 对类名进行遍历
        for (String className : classNameList) {
            ClassImplementsMethodInfo classImplementsMethodInfo = classInterfaceMethodInfoMap.get(className);
            List<String> interfaceNameList = classImplementsMethodInfo.getInterfaceNameList();
            // 对实现的接口进行排序
            Collections.sort(interfaceNameList);

            // 找到在接口和实现类中都存在的
            for (String interfaceName : interfaceNameList) {
                List<MethodAndArgs> interfaceMethodWithArgsList = interfaceMethodWithArgsMap.get(interfaceName);
                if (interfaceMethodWithArgsList == null || interfaceMethodWithArgsList.isEmpty()) {
                    continue;
                }

                List<MethodAndArgs> methodWithArgsList = classImplementsMethodInfo.getMethodWithArgsList();
                // 对方法进行排序
                methodWithArgsList.sort(MethodAndArgsComparator.getInstance());
                // 对方法进行遍历
                for (MethodAndArgs methodWithArgs : methodWithArgsList) {
                    if (!interfaceMethodWithArgsList.contains(methodWithArgs)) {
                        // 接口中不包含的方法，跳过
                        continue;
                    }

                    addExtraMethodCall(methodCallWriter, interfaceName, methodWithArgs.getMethodName(), methodWithArgs.getMethodArgs(), CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS,
                            className, methodWithArgs.getMethodName(), methodWithArgs.getMethodArgs());
                }
            }
        }
    }

    // 添加额外的方法调用关系
    private void addExtraMethodCall(Writer methodCallWriter,
                                    String callerClassName,
                                    String callerMethodName,
                                    String callerMethodArgs,
                                    CallTypeEnum methodCallType,
                                    String calleeClassName,
                                    String calleeMethodName,
                                    String calleeMethodArgs) throws IOException {
        if (JavaCGUtil.checkSkipClass(callerClassName, javaCGConfInfo.getNeedHandlePackageSet()) ||
                JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return;
        }

        MethodCall methodCall = new MethodCall(callIdCounter.addAndGet(), callerClassName, callerMethodName,
                callerMethodArgs, methodCallType, calleeClassName, calleeMethodName, calleeMethodArgs, JavaCGConstants.DEFAULT_LINE_NUMBER);
        JavaCGFileUtil.write2FileWithTab(methodCallWriter, methodCall.genCallContent(), JavaCGConstants.DEFAULT_JAR_NUM);
    }

    //
    public void setJavaCGConfInfo(JavaCGConfInfo javaCGConfInfo) {
        this.javaCGConfInfo = javaCGConfInfo;
    }

    public void setCallIdCounter(JavaCGCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setInterfaceMethodWithArgsMap(Map<String, List<MethodAndArgs>> interfaceMethodWithArgsMap) {
        this.interfaceMethodWithArgsMap = interfaceMethodWithArgsMap;
    }

    public void setChildrenClassMap(Map<String, List<String>> childrenClassMap) {
        this.childrenClassMap = childrenClassMap;
    }

    public void setInterfaceExtendsMethodInfoMap(Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap) {
        this.interfaceExtendsMethodInfoMap = interfaceExtendsMethodInfoMap;
    }

    public void setChildrenInterfaceMap(Map<String, List<String>> childrenInterfaceMap) {
        this.childrenInterfaceMap = childrenInterfaceMap;
    }

    public void setClassInterfaceMethodInfoMap(Map<String, ClassImplementsMethodInfo> classInterfaceMethodInfoMap) {
        this.classInterfaceMethodInfoMap = classInterfaceMethodInfoMap;
    }

    public void setClassExtendsMethodInfoMap(Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap) {
        this.classExtendsMethodInfoMap = classExtendsMethodInfoMap;
    }
}
