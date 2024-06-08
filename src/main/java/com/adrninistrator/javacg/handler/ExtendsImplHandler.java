package com.adrninistrator.javacg.handler;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.comparator.MethodArgReturnTypesComparator;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.accessflag.JavaCGAccessFlags;
import com.adrninistrator.javacg.dto.call.MethodCall;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.classes.Node4ClassExtendsMethod;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.dto.interfaces.InterfaceExtendsMethodInfo;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg.dto.stack.ListAsStack;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    private static final Logger logger = LoggerFactory.getLogger(ExtendsImplHandler.class);

    private JavaCGConfInfo javaCGConfInfo;

    private JavaCGCounter callIdCounter;

    private Map<String, List<MethodArgReturnTypes>> interfaceMethodWithArgTypesMap;
    private Map<String, List<String>> childrenClassMap;
    private Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap;
    private Map<String, List<String>> childrenInterfaceMap;
    private Map<String, ClassImplementsMethodInfo> classImplementsMethodInfoMap;
    private Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap;
    private Set<String> allClassNameSet;

    private ClassAndJarNum classAndJarNum;

    public void handle(Writer methodCallWriter) throws IOException {
        // 将父接口中的方法添加到子接口中
        addSuperInterfaceMethod4ChildrenInterface(methodCallWriter);

        // 将接口中的抽象方法添加到抽象父类中
        addInterfaceMethod4SuperAbstractClass();

        // 记录父类调用子类方法，及子类调用父类方法
        recordClassExtendsMethod(methodCallWriter);

        // 记录接口调用实现类方法
        recordInterfaceCallClassMethod(methodCallWriter);
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
                    logger.debug("处理一个顶层父接口: {}", superInterface);
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

        List<MethodArgReturnTypes> superInterfaceMethodAndArgsList = superInterfaceExtendsMethodInfo.getMethodAndArgTypesList();
        // 对父接口中的方法进行排序
        superInterfaceMethodAndArgsList.sort(MethodArgReturnTypesComparator.getInstance());
        // 遍历父接口中的方法
        for (MethodArgReturnTypes superMethodAndArgs : superInterfaceMethodAndArgsList) {
            List<MethodArgReturnTypes> childInterfaceMethodAndArgsList = childInterfaceExtendsMethodInfo.getMethodAndArgTypesList();
            if (childInterfaceMethodAndArgsList.contains(superMethodAndArgs)) {
                // 子接口中已包含父接口，跳过
                continue;
            }

            // 在子接口中添加父接口的方法（涉及继承的接口相关结构）
            childInterfaceMethodAndArgsList.add(superMethodAndArgs);

            // 在子接口中添加父接口的方法（所有接口都需要记录的结构）
            List<MethodArgReturnTypes> childInterfaceMethodAndArgsListAll = interfaceMethodWithArgTypesMap.computeIfAbsent(childInterface, k -> new ArrayList<>());
            childInterfaceMethodAndArgsListAll.add(superMethodAndArgs);

            // 添加子接口调用父接口方法
            addExtraMethodCall(methodCallWriter, childInterface, superMethodAndArgs.getMethodName(), superMethodAndArgs.getMethodArgTypes(),
                    superMethodAndArgs.getMethodReturnType(), JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER_INTERFACE, superInterface, superMethodAndArgs.getMethodName(),
                    superMethodAndArgs.getMethodArgTypes(), superMethodAndArgs.getMethodReturnType());
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

            ClassImplementsMethodInfo classImplementsMethodInfo = classImplementsMethodInfoMap.get(superClassName);
            if (classImplementsMethodInfo == null) {
                continue;
            }

            Map<MethodArgReturnTypes, Integer> methodWithArgTypesMap = classExtendsMethodInfo.getMethodWithArgTypesMap();

            int accessFlags = 0;
            accessFlags = JavaCGByteCodeUtil.setAbstractFlag(accessFlags, true);
            accessFlags = JavaCGByteCodeUtil.setPublicFlag(accessFlags, true);
            accessFlags = JavaCGByteCodeUtil.setProtectedFlag(accessFlags, false);

            // 将接口中的抽象方法加到抽象父类中
            for (String interfaceName : classImplementsMethodInfo.getInterfaceNameList()) {
                List<MethodArgReturnTypes> interfaceMethodWithArgTypesList = interfaceMethodWithArgTypesMap.get(interfaceName);
                if (interfaceMethodWithArgTypesList == null) {
                    continue;
                }

                for (MethodArgReturnTypes interfaceMethodWithArgTypes : interfaceMethodWithArgTypesList) {
                    // 添加时不覆盖现有的值
                    methodWithArgTypesMap.putIfAbsent(interfaceMethodWithArgTypes, accessFlags);
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
            // 判断当前类是否为顶层父类
            if (checkTopSuperClass(className, superClassName)) {
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

    // 判断某个类是否为顶层父类
    private boolean checkTopSuperClass(String className, String superClassName) {
        if (!JavaCGClassMethodUtil.isClassInJdk(className) && JavaCGClassMethodUtil.isClassInJdk(superClassName)) {
            // 当前类不是JDK中的类，且父类是JDK中的类，属于顶层父类
            return true;
        }
        // 若当前类是JDK中的类，且父类不在所有处理的jar包中，属于顶层父类
        return JavaCGClassMethodUtil.isClassInJdk(className) && !allClassNameSet.contains(superClassName);
    }

    // 处理一个顶层父类
    private void handleOneTopSuperClass(String topSuperClassName, Writer methodCallWriter) throws IOException {
        logger.debug("处理一个顶层父类: {}", topSuperClassName);
        ListAsStack<Node4ClassExtendsMethod> nodeStack = new ListAsStack<>();

        // 初始化节点列表
        nodeStack.push(new Node4ClassExtendsMethod(topSuperClassName, -1));

        // 开始循环
        while (true) {
            Node4ClassExtendsMethod currentNode = nodeStack.peek();
            List<String> childrenClassList = childrenClassMap.get(currentNode.getSuperClassName());
            if (childrenClassList == null) {
                logger.debug("未找到顶层父类的子类: {}", currentNode.getSuperClassName());
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
            nodeStack.push(new Node4ClassExtendsMethod(childClassName, -1));
        }
    }

    // 处理父类和子类的方法调用
    private void handleSuperAndChildClass(String superClassName, String childClassName, Writer methodCallWriter) throws IOException {
        ClassExtendsMethodInfo superClassMethodInfo = classExtendsMethodInfoMap.get(superClassName);
        if (superClassMethodInfo == null) {
            logger.error("未找到父类信息: {}", superClassName);
            return;
        }

        ClassExtendsMethodInfo childClassMethodInfo = classExtendsMethodInfoMap.get(childClassName);
        if (childClassMethodInfo == null) {
            logger.error("未找到子类信息: {}", childClassName);
            return;
        }

        Map<MethodArgReturnTypes, Integer> superMethodWithArgTypesMap = superClassMethodInfo.getMethodWithArgTypesMap();
        Map<MethodArgReturnTypes, Integer> childMethodWithArgTypesMap = childClassMethodInfo.getMethodWithArgTypesMap();

        List<MethodArgReturnTypes> superMethodAndArgTypesList = new ArrayList<>(superMethodWithArgTypesMap.keySet());
        // 对父类方法进行排序
        superMethodAndArgTypesList.sort(MethodArgReturnTypesComparator.getInstance());
        // 遍历父类方法
        for (MethodArgReturnTypes superMethodWithArgTypes : superMethodAndArgTypesList) {
            Integer superMethodAccessFlags = superMethodWithArgTypesMap.get(superMethodWithArgTypes);
            if (JavaCGByteCodeUtil.isAbstractFlag(superMethodAccessFlags)) {
                // 处理父类抽象方法
                // 添加时不覆盖现有的值
                childMethodWithArgTypesMap.putIfAbsent(superMethodWithArgTypes, superMethodAccessFlags);
                // 添加父类调用子类的方法调用
                addExtraMethodCall(methodCallWriter, superClassName, superMethodWithArgTypes.getMethodName(), superMethodWithArgTypes.getMethodArgTypes(),
                        superMethodWithArgTypes.getMethodReturnType(), JavaCGCallTypeEnum.CTE_SUPER_CALL_CHILD, childClassName, superMethodWithArgTypes.getMethodName(),
                        superMethodWithArgTypes.getMethodArgTypes(), superMethodWithArgTypes.getMethodReturnType());
                continue;
            }

            if (JavaCGByteCodeUtil.isPublicFlag(superMethodAccessFlags)
                    || JavaCGByteCodeUtil.isProtectedMethod(superMethodAccessFlags)
                    || (!JavaCGByteCodeUtil.isPrivateMethod(superMethodAccessFlags)
                    && JavaCGClassMethodUtil.checkSamePackage(superClassName, childClassName))
            ) {
                /*
                    对于父类中满足以下条件的非抽象方法进行处理：
                    public
                    或protected
                    或非public非protected非private且父类与子类在同一个包
                 */
                if (childMethodWithArgTypesMap.get(superMethodWithArgTypes) != null) {
                    // 若当前方法已经处理过则跳过
                    continue;
                }

                childMethodWithArgTypesMap.put(superMethodWithArgTypes, superMethodAccessFlags);
                // 添加子类调用父类方法
                addExtraMethodCall(methodCallWriter, childClassName, superMethodWithArgTypes.getMethodName(), superMethodWithArgTypes.getMethodArgTypes(),
                        superMethodWithArgTypes.getMethodReturnType(), JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER, superClassName, superMethodWithArgTypes.getMethodName(),
                        superMethodWithArgTypes.getMethodArgTypes(), superMethodWithArgTypes.getMethodReturnType());
            }
        }
    }

    // 记录接口调用实现类方法
    private void recordInterfaceCallClassMethod(Writer methodCallWriter) throws IOException {
        if (classImplementsMethodInfoMap.isEmpty() || interfaceMethodWithArgTypesMap.isEmpty()) {
            return;
        }

        List<String> classNameList = new ArrayList<>(classImplementsMethodInfoMap.keySet());
        // 对类名进行排序
        Collections.sort(classNameList);
        // 对类名进行遍历
        for (String className : classNameList) {
            ClassImplementsMethodInfo classImplementsMethodInfo = classImplementsMethodInfoMap.get(className);
            List<String> interfaceNameList = classImplementsMethodInfo.getInterfaceNameList();
            // 对实现的接口进行排序
            Collections.sort(interfaceNameList);

            // 找到在接口和实现类中都存在的方法
            for (String interfaceName : interfaceNameList) {
                List<MethodArgReturnTypes> interfaceMethodWithArgTypesList = interfaceMethodWithArgTypesMap.get(interfaceName);
                if (JavaCGUtil.isCollectionEmpty(interfaceMethodWithArgTypesList)) {
                    continue;
                }

                List<MethodArgReturnTypes> methodWithArgTypesList = classImplementsMethodInfo.getMethodWithArgTypesList();
                // 在处理接口调用实现类方法时，将父类中定义的可能涉及实现的方法添加到当前类的方法中
                addSuperMethod2ImplClass(methodWithArgTypesList, className);

                // 对方法进行排序
                methodWithArgTypesList.sort(MethodArgReturnTypesComparator.getInstance());
                // 对方法进行遍历
                for (MethodArgReturnTypes methodWithArgTypes : methodWithArgTypesList) {
                    if (!interfaceMethodWithArgTypesList.contains(methodWithArgTypes)) {
                        // 接口中不包含的方法，跳过
                        continue;
                    }
                    // 添加接口调用实现类方法
                    addExtraMethodCall(methodCallWriter, interfaceName, methodWithArgTypes.getMethodName(), methodWithArgTypes.getMethodArgTypes(),
                            methodWithArgTypes.getMethodReturnType(),
                            JavaCGCallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS, className, methodWithArgTypes.getMethodName(), methodWithArgTypes.getMethodArgTypes(),
                            methodWithArgTypes.getMethodReturnType());
                }
            }
        }
    }

    /**
     * 在处理接口调用实现类方法时，将父类中定义的可能涉及实现的方法添加到当前类的方法中
     *
     * @param methodWithArgTypesList 当前类中定义的方法
     * @param className
     */
    private void addSuperMethod2ImplClass(List<MethodArgReturnTypes> methodWithArgTypesList, String className) {
        // 获取当前处理的实现类涉及继承的信息
        ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoMap.get(className);
        if (classExtendsMethodInfo == null) {
            return;
        }

        // 获取当前处理的实现类中的方法信息
        Map<MethodArgReturnTypes, Integer> methodWithArgTypesMap = classExtendsMethodInfo.getMethodWithArgTypesMap();
        if (methodWithArgTypesMap == null) {
            return;
        }

        for (Map.Entry<MethodArgReturnTypes, Integer> entry : methodWithArgTypesMap.entrySet()) {
            MethodArgReturnTypes methodAndArgTypes = entry.getKey();
            if (methodWithArgTypesList.contains(methodAndArgTypes)) {
                // 已包含的方法，跳过
                continue;
            }

            String methodName = methodAndArgTypes.getMethodName();
            JavaCGAccessFlags methodAccessFlags = new JavaCGAccessFlags(entry.getValue());
            if (JavaCGByteCodeUtil.checkImplMethod(methodName, methodAccessFlags)) {
                // 将父类中定义的，可能涉及实现的方法添加到当前类的方法中
                methodWithArgTypesList.add(methodAndArgTypes);
            }
        }
    }

    // 添加额外的方法调用关系
    private void addExtraMethodCall(Writer methodCallWriter,
                                    String callerClassName,
                                    String callerMethodName,
                                    String callerMethodArgTypes,
                                    String callerMethodReturnType,
                                    JavaCGCallTypeEnum methodCallType,
                                    String calleeClassName,
                                    String calleeMethodName,
                                    String calleeMethodArgTypes,
                                    String calleeMethodReturnType) throws IOException {
        if (JavaCGUtil.checkSkipClass(callerClassName, javaCGConfInfo.getNeedHandlePackageSet()) ||
                JavaCGUtil.checkSkipClass(calleeClassName, javaCGConfInfo.getNeedHandlePackageSet())) {
            return;
        }

        String callerClassJarNum = classAndJarNum.getJarNum(callerClassName);
        String calleeClassJarNum = classAndJarNum.getJarNum(calleeClassName);

        MethodCall methodCall = new MethodCall();
        methodCall.setCallId(callIdCounter.addAndGet());
        methodCall.setCallerClassName(callerClassName);
        methodCall.setCallerMethodName(callerMethodName);
        methodCall.setCallerMethodArgTypes(callerMethodArgTypes);
        methodCall.setCallerReturnType(callerMethodReturnType);
        methodCall.setCallerSourceLine(JavaCGConstants.DEFAULT_LINE_NUMBER);
        methodCall.setMethodCallType(methodCallType);
        methodCall.setCalleeClassName(calleeClassName);
        methodCall.setCalleeMethodName(calleeMethodName);
        methodCall.setCalleeMethodArgTypes(calleeMethodArgTypes);
        methodCall.setRawReturnType(calleeMethodReturnType);
        JavaCGFileUtil.write2FileWithTab(methodCallWriter, methodCall.genMethodCallContent(callerClassJarNum, calleeClassJarNum));
    }

    //
    public void setJavaCGConfInfo(JavaCGConfInfo javaCGConfInfo) {
        this.javaCGConfInfo = javaCGConfInfo;
    }

    public void setCallIdCounter(JavaCGCounter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setInterfaceMethodWithArgTypesMap(Map<String, List<MethodArgReturnTypes>> interfaceMethodWithArgTypesMap) {
        this.interfaceMethodWithArgTypesMap = interfaceMethodWithArgTypesMap;
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

    public void setClassImplementsMethodInfoMap(Map<String, ClassImplementsMethodInfo> classImplementsMethodInfoMap) {
        this.classImplementsMethodInfoMap = classImplementsMethodInfoMap;
    }

    public void setClassExtendsMethodInfoMap(Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap) {
        this.classExtendsMethodInfoMap = classExtendsMethodInfoMap;
    }

    public void setAllClassNameSet(Set<String> allClassNameSet) {
        this.allClassNameSet = allClassNameSet;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
