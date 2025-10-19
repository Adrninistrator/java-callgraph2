package com.adrninistrator.javacg2.handler;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.comparator.Comparator4MethodArgReturnTypes;
import com.adrninistrator.javacg2.dto.accessflag.JavaCG2AccessFlags;
import com.adrninistrator.javacg2.dto.call.MethodCall;
import com.adrninistrator.javacg2.dto.classes.ClassExtendsInfo;
import com.adrninistrator.javacg2.dto.classes.Node4ClassExtendsMethod;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg2.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg2.dto.stack.ListAsStack;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;
import com.adrninistrator.javacg2.util.JavaCG2ByteCodeUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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

    private final JavaCG2ElManager javaCG2ElManager;

    private JavaCG2Counter callIdCounter;

    private Map<String, Map<MethodArgReturnTypes, Integer>> interfaceMethodWithArgTypesMap;
    private Map<String, Map<MethodArgReturnTypes, Integer>> classExtendsImplMethodWithArgTypesMap;
    private Map<String, List<String>> childrenClassMap;
    private Map<String, List<String>> interfaceExtendsInfoMap;
    private Map<String, List<String>> childrenInterfaceMap;
    private Map<String, List<String>> classImplementsInfoMap;
    private Map<String, ClassExtendsInfo> classExtendsInfoMap;
    private Set<String> allClassNameSet;

    private ClassAndJarNum classAndJarNum;

    private Writer methodCallWriter;

    public ExtendsImplHandler(JavaCG2InputAndOutput javaCG2InputAndOutput) {
        javaCG2ElManager = javaCG2InputAndOutput.getJavaCG2ElManager();
    }

    public void handle() throws IOException {
        // 将父接口中的方法添加到子接口中
        addSuperInterfaceMethod4ChildrenInterface();

        // 将接口中的抽象方法添加到抽象父类中
        addInterfaceMethod4SuperAbstractClass();

        // 将有实现接口的类的父类中定义的可能涉及实现的方法添加到当前类的方法中，在以下处理类的实现、继承关系之前执行
        addSuperMethod2ImplClass();

        // 记录并添加接口中的非抽象方法到实现类中，在以下处理类的继承关系之前执行
        recordInterfaceDefaultMethod4ImplClass();

        // 记录父类调用子类方法，及子类调用父类方法
        recordClassExtendsMethod();

        // 记录接口调用实现类方法
        recordInterfaceCallClassMethod();
    }

    // 将父接口中的方法添加到子接口中
    private void addSuperInterfaceMethod4ChildrenInterface() throws IOException {
        // 查找顶层父接口
        Set<String> topSuperInterfaceSet = new HashSet<>();
        for (Map.Entry<String, List<String>> entry : interfaceExtendsInfoMap.entrySet()) {
            for (String superInterface : entry.getValue()) {
                List<String> superInterfaceListOfSuper = interfaceExtendsInfoMap.get(superInterface);
                if (JavaCG2Util.isCollectionEmpty(superInterfaceListOfSuper)) {
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
            handleOneSuperInterface(topSuperInterface);
        }
    }

    // 处理一个父接口
    private void handleOneSuperInterface(String superInterface) throws IOException {
        List<String> childrenInterfaceList = childrenInterfaceMap.get(superInterface);
        if (childrenInterfaceList == null) {
            return;
        }

        for (String childrenInterface : childrenInterfaceList) {
            // 处理父接口及一个子接口
            handleSuperAndChildInterface(superInterface, childrenInterface);

            // 继续处理子接口
            handleOneSuperInterface(childrenInterface);
        }
    }

    // 处理父接口及一个子接口
    private void handleSuperAndChildInterface(String superInterface, String childInterface) throws IOException {
        List<String> superInterfaceList = interfaceExtendsInfoMap.get(superInterface);
        if (superInterfaceList == null) {
            // 父接口在接口涉及继承的信息Map中不存在记录时，不处理
            return;
        }

        Map<MethodArgReturnTypes, Integer> superInterfaceMethodAndArgsMap = interfaceMethodWithArgTypesMap.get(superInterface);
        if (JavaCG2Util.isMapEmpty(superInterfaceMethodAndArgsMap)) {
            // 父接口中不存在方法，不处理
            return;
        }
        Map<MethodArgReturnTypes, Integer> childInterfaceMethodAndArgsMap = interfaceMethodWithArgTypesMap.computeIfAbsent(childInterface, k -> new HashMap<>());

        List<MethodArgReturnTypes> superInterfaceMethodAndArgsList = new ArrayList<>(superInterfaceMethodAndArgsMap.keySet());
        // 对父接口中的方法进行排序
        superInterfaceMethodAndArgsList.sort(Comparator4MethodArgReturnTypes.getInstance());
        // 遍历父接口中的方法
        for (MethodArgReturnTypes superMethodAndArgs : superInterfaceMethodAndArgsList) {
            if (childInterfaceMethodAndArgsMap.containsKey(superMethodAndArgs)) {
                // 子接口中已包含父接口的方法，跳过
                continue;
            }

            Integer superMethodAccessFlags = superInterfaceMethodAndArgsMap.get(superMethodAndArgs);
            // 在子接口中添加父接口的方法
            childInterfaceMethodAndArgsMap.put(superMethodAndArgs, superMethodAccessFlags);

            JavaCG2CallTypeEnum callTypeEnum = JavaCG2ByteCodeUtil.isAbstractFlag(superMethodAccessFlags) ? JavaCG2CallTypeEnum.CTE_CHILD_CALL_SUPER_INTERFACE :
                    JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_INTERFACE_DEFAULT;
            // 添加子接口调用父接口方法
            addExtraMethodCall(childInterface, superMethodAndArgs.getMethodName(), superMethodAndArgs.getMethodArgTypes(),
                    superMethodAndArgs.getMethodReturnType(), callTypeEnum, superInterface, superMethodAndArgs.getMethodName(),
                    superMethodAndArgs.getMethodArgTypes(), superMethodAndArgs.getMethodReturnType());
        }
    }

    // 将接口中的抽象方法加到抽象父类中
    private void addInterfaceMethod4SuperAbstractClass() {
        for (Map.Entry<String, List<String>> childrenClassEntry : childrenClassMap.entrySet()) {
            String superClassName = childrenClassEntry.getKey();
            ClassExtendsInfo classExtendsInfo = classExtendsInfoMap.get(superClassName);
            if (classExtendsInfo == null || !JavaCG2ByteCodeUtil.isAbstractFlag(classExtendsInfo.getAccessFlags())) {
                /*
                    为空的情况，对应其他jar包中的Class可以找到，但是找不到它们的方法，是正常的，不处理
                    若不是抽象类则不处理
                 */
                continue;
            }

            List<String> classImplementsInterfaceList = classImplementsInfoMap.get(superClassName);
            if (classImplementsInterfaceList == null) {
                continue;
            }

            Map<MethodArgReturnTypes, Integer> superCLassMethodWithArgTypesMap = classExtendsImplMethodWithArgTypesMap.computeIfAbsent(superClassName, k -> new HashMap<>());

            int accessFlags = 0;
            accessFlags = JavaCG2ByteCodeUtil.setAbstractFlag(accessFlags, true);
            accessFlags = JavaCG2ByteCodeUtil.setPublicFlag(accessFlags, true);
            accessFlags = JavaCG2ByteCodeUtil.setProtectedFlag(accessFlags, false);

            // 将接口中的抽象方法加到抽象父类中
            for (String interfaceName : classImplementsInterfaceList) {
                Map<MethodArgReturnTypes, Integer> currentInterfaceMethodWithArgTypesMap = interfaceMethodWithArgTypesMap.get(interfaceName);
                if (JavaCG2Util.isMapEmpty(currentInterfaceMethodWithArgTypesMap)) {
                    continue;
                }

                for (Map.Entry<MethodArgReturnTypes, Integer> entry : currentInterfaceMethodWithArgTypesMap.entrySet()) {
                    if (!JavaCG2ByteCodeUtil.isAbstractFlag(entry.getValue())) {
                        continue;
                    }
                    /*
                        添加时不覆盖现有的值
                        添加的方法access_flags需要满足能够被继承的要求
                     */
                    superCLassMethodWithArgTypesMap.putIfAbsent(entry.getKey(), accessFlags);
                }
            }
        }
    }

    // 将有实现接口的类的父类中定义的可能涉及实现的方法添加到当前类的方法中，在以下处理类的实现、继承关系之前执行
    private void addSuperMethod2ImplClass() {
        if (classImplementsInfoMap.isEmpty()) {
            return;
        }

        for (Map.Entry<String, List<String>> classImplementsEntry : classImplementsInfoMap.entrySet()) {
            String className = classImplementsEntry.getKey();
            // 获取当前处理的实现类涉及继承的信息
            ClassExtendsInfo classExtendsInfo = classExtendsInfoMap.get(className);
            if (classExtendsInfo == null) {
                return;
            }

            // 获取当前处理的实现类中涉及继承的方法信息
            Map<MethodArgReturnTypes, Integer> methodWithArgTypesMapExtends = classExtendsImplMethodWithArgTypesMap.get(className);
            if (methodWithArgTypesMapExtends == null) {
                return;
            }

            Map<MethodArgReturnTypes, Integer> methodWithArgTypesMap = classExtendsImplMethodWithArgTypesMap.computeIfAbsent(className, k -> new HashMap<>());
            for (Map.Entry<MethodArgReturnTypes, Integer> entry : methodWithArgTypesMapExtends.entrySet()) {
                MethodArgReturnTypes methodAndArgTypes = entry.getKey();
                if (methodWithArgTypesMap.containsKey(methodAndArgTypes)) {
                    // 已包含的方法，跳过
                    continue;
                }

                String methodName = methodAndArgTypes.getMethodName();
                JavaCG2AccessFlags methodAccessFlags = new JavaCG2AccessFlags(entry.getValue());
                if (JavaCG2ByteCodeUtil.checkImplMethod(methodName, methodAccessFlags)) {
                    // 将父类中定义的，可能涉及实现的方法添加到当前类的方法中
                    methodWithArgTypesMap.put(methodAndArgTypes, entry.getValue());
                }
            }
        }
    }

    // 将接口中的非抽象方法添加到实现类中，在以下处理类的继承关系之前执行
    private void recordInterfaceDefaultMethod4ImplClass() throws IOException {
        if (classImplementsInfoMap.isEmpty() || interfaceMethodWithArgTypesMap.isEmpty()) {
            return;
        }

        // 处理类实现的接口信息
        List<String> classNameList = new ArrayList<>(classImplementsInfoMap.keySet());
        Collections.sort(classNameList);
        for (String className : classNameList) {
            List<String> interfaceNameList = classImplementsInfoMap.get(className);

            // 找到接口中的default方法，且在实现类中不存在的
            for (String interfaceName : interfaceNameList) {
                Map<MethodArgReturnTypes, Integer> currentInterfaceMethodWithArgTypesMap = interfaceMethodWithArgTypesMap.get(interfaceName);
                if (JavaCG2Util.isMapEmpty(currentInterfaceMethodWithArgTypesMap)) {
                    continue;
                }
                List<MethodArgReturnTypes> interfaceMethodArgReturnTypesList = new ArrayList<>(currentInterfaceMethodWithArgTypesMap.keySet());
                interfaceMethodArgReturnTypesList.sort(Comparator4MethodArgReturnTypes.getInstance());
                for (MethodArgReturnTypes interfaceMethodArgReturnTypes : interfaceMethodArgReturnTypesList) {
                    Integer interfaceMethodAccessFlags = currentInterfaceMethodWithArgTypesMap.get(interfaceMethodArgReturnTypes);
                    if (JavaCG2ByteCodeUtil.isAbstractFlag(interfaceMethodAccessFlags)) {
                        // 跳过接口中非default方法
                        continue;
                    }
                    Map<MethodArgReturnTypes, Integer> classMethodArgReturnTypesMap = classExtendsImplMethodWithArgTypesMap.computeIfAbsent(className, k -> new HashMap<>());
                    if (!classMethodArgReturnTypesMap.containsKey(interfaceMethodArgReturnTypes)) {
                        // 当实现类中不存在接口default方法时，添加到实现类中
                        classMethodArgReturnTypesMap.putIfAbsent(interfaceMethodArgReturnTypes, interfaceMethodAccessFlags);
                        // 添加实现类调用接口default方法调用
                        addExtraMethodCall(className, interfaceMethodArgReturnTypes.getMethodName(), interfaceMethodArgReturnTypes.getMethodArgTypes(),
                                interfaceMethodArgReturnTypes.getMethodReturnType(), JavaCG2CallTypeEnum.CTE_CLASS_CALL_INTERFACE_DEFAULT, interfaceName,
                                interfaceMethodArgReturnTypes.getMethodName(), interfaceMethodArgReturnTypes.getMethodArgTypes(),
                                interfaceMethodArgReturnTypes.getMethodReturnType());
                    }
                }
            }
        }
    }

    // 记录父类调用子类方法，及子类调用父类方法
    private void recordClassExtendsMethod() throws IOException {
        if (classExtendsInfoMap.isEmpty()) {
            return;
        }

        Set<String> topSuperClassNameSet = new HashSet<>();

        // 得到最顶层父类名称
        for (Map.Entry<String, ClassExtendsInfo> classExtendsMethodInfoEntry : classExtendsInfoMap.entrySet()) {
            String className = classExtendsMethodInfoEntry.getKey();
            ClassExtendsInfo classExtendsInfo = classExtendsMethodInfoEntry.getValue();
            String superClassName = classExtendsInfo.getSuperClassName();
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
            handleOneTopSuperClass(topSuperClassName);
        }
    }

    // 判断某个类是否为顶层父类
    private boolean checkTopSuperClass(String className, String superClassName) {
        if (!JavaCG2ClassMethodUtil.isClassInJdk(className) && JavaCG2ClassMethodUtil.isClassInJdk(superClassName)) {
            // 当前类不是JDK中的类，且父类是JDK中的类，属于顶层父类
            return true;
        }
        // 若当前类是JDK中的类，且父类不在所有处理的jar包中，属于顶层父类
        return JavaCG2ClassMethodUtil.isClassInJdk(className) && !allClassNameSet.contains(superClassName);
    }

    // 处理一个顶层父类
    private void handleOneTopSuperClass(String topSuperClassName) throws IOException {
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
            handleSuperAndChildClass(currentNode.getSuperClassName(), childClassName);

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
    private void handleSuperAndChildClass(String superClassName, String childClassName) throws IOException {
        Map<MethodArgReturnTypes, Integer> superMethodWithArgTypesMap = classExtendsImplMethodWithArgTypesMap.get(superClassName);
        if (JavaCG2Util.isMapEmpty(superMethodWithArgTypesMap)) {
            return;
        }
        Map<MethodArgReturnTypes, Integer> childMethodWithArgTypesMap = classExtendsImplMethodWithArgTypesMap.computeIfAbsent(childClassName, k -> new HashMap<>());

        // 对父类方法排序并遍历
        List<MethodArgReturnTypes> superMethodAndArgTypesList = new ArrayList<>(superMethodWithArgTypesMap.keySet());
        superMethodAndArgTypesList.sort(Comparator4MethodArgReturnTypes.getInstance());
        for (MethodArgReturnTypes superMethodWithArgTypes : superMethodAndArgTypesList) {
            Integer superMethodAccessFlags = superMethodWithArgTypesMap.get(superMethodWithArgTypes);
            if (JavaCG2ByteCodeUtil.isAbstractFlag(superMethodAccessFlags)) {
                // 处理父类抽象方法
                // 将父类方法添加到子类，添加时不覆盖现有的值
                childMethodWithArgTypesMap.putIfAbsent(superMethodWithArgTypes, superMethodAccessFlags);
                // 添加父类调用子类的方法调用
                addExtraMethodCall(superClassName, superMethodWithArgTypes.getMethodName(), superMethodWithArgTypes.getMethodArgTypes(),
                        superMethodWithArgTypes.getMethodReturnType(), JavaCG2CallTypeEnum.CTE_SUPER_CALL_CHILD, childClassName, superMethodWithArgTypes.getMethodName(),
                        superMethodWithArgTypes.getMethodArgTypes(), superMethodWithArgTypes.getMethodReturnType());
                continue;
            }

            if (JavaCG2ByteCodeUtil.isPublicFlag(superMethodAccessFlags)
                    || JavaCG2ByteCodeUtil.isProtectedMethod(superMethodAccessFlags)
                    || (!JavaCG2ByteCodeUtil.isPrivateMethod(superMethodAccessFlags)
                    && JavaCG2ClassMethodUtil.checkSamePackage(superClassName, childClassName))
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
                addExtraMethodCall(childClassName, superMethodWithArgTypes.getMethodName(), superMethodWithArgTypes.getMethodArgTypes(),
                        superMethodWithArgTypes.getMethodReturnType(), JavaCG2CallTypeEnum.CTE_CHILD_CALL_SUPER, superClassName, superMethodWithArgTypes.getMethodName(),
                        superMethodWithArgTypes.getMethodArgTypes(), superMethodWithArgTypes.getMethodReturnType());
            }
        }
    }

    // 记录接口调用实现类方法
    private void recordInterfaceCallClassMethod() throws IOException {
        if (classImplementsInfoMap.isEmpty() || interfaceMethodWithArgTypesMap.isEmpty()) {
            return;
        }

        // 处理类实现的方法信息
        List<String> classNameList = new ArrayList<>(classImplementsInfoMap.keySet());
        Collections.sort(classNameList);
        for (String className : classNameList) {
            List<String> interfaceNameList = classImplementsInfoMap.get(className);
            Collections.sort(interfaceNameList);

            // 获得当前类的方法并排序
            Map<MethodArgReturnTypes, Integer> classMethodWithArgTypesMap = classExtendsImplMethodWithArgTypesMap.get(className);
            if (JavaCG2Util.isMapEmpty(classMethodWithArgTypesMap)) {
                continue;
            }

            List<MethodArgReturnTypes> methodWithArgTypesList = new ArrayList<>(classMethodWithArgTypesMap.keySet());
            methodWithArgTypesList.sort(Comparator4MethodArgReturnTypes.getInstance());

            for (String interfaceName : interfaceNameList) {
                Map<MethodArgReturnTypes, Integer> currentInterfaceMethodWithArgTypesMap = interfaceMethodWithArgTypesMap.get(interfaceName);
                if (JavaCG2Util.isMapEmpty(currentInterfaceMethodWithArgTypesMap)) {
                    continue;
                }

                // 找到在接口和实现类中都存在的方法
                for (MethodArgReturnTypes methodWithArgTypes : methodWithArgTypesList) {
                    Integer currentInterfaceMethodAccessFlags = currentInterfaceMethodWithArgTypesMap.get(methodWithArgTypes);
                    if (currentInterfaceMethodAccessFlags == null || !JavaCG2ByteCodeUtil.isAbstractFlag(currentInterfaceMethodAccessFlags)) {
                        // 接口中不包含的方法，或default方法，跳过
                        continue;
                    }
                    // 添加接口调用实现类方法
                    addExtraMethodCall(interfaceName, methodWithArgTypes.getMethodName(), methodWithArgTypes.getMethodArgTypes(),
                            methodWithArgTypes.getMethodReturnType(), JavaCG2CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS, className, methodWithArgTypes.getMethodName(),
                            methodWithArgTypes.getMethodArgTypes(), methodWithArgTypes.getMethodReturnType());
                }
            }
        }
    }

    // 添加额外的方法调用关系
    private void addExtraMethodCall(String callerClassName,
                                    String callerMethodName,
                                    String callerMethodArgTypes,
                                    String callerMethodReturnType,
                                    JavaCG2CallTypeEnum methodCallType,
                                    String calleeClassName,
                                    String calleeMethodName,
                                    String calleeMethodArgTypes,
                                    String calleeMethodReturnType) throws IOException {
        String callerClassJarNum = classAndJarNum.getJarNum(callerClassName);
        String calleeClassJarNum = classAndJarNum.getJarNum(calleeClassName);

        String calleeFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(calleeClassName, calleeMethodName, calleeMethodArgTypes);
        String callerFullMethod = JavaCG2ClassMethodUtil.formatFullMethod(callerClassName, callerMethodName, callerMethodArgTypes);
        // 判断当前方法调用是否需要忽略
        if (javaCG2ElManager.checkIgnoreMethodCall(methodCallType.getType(), callerFullMethod, calleeFullMethod)) {
            return;
        }

        MethodCall methodCall = new MethodCall();
        methodCall.setCallId(callIdCounter.addAndGet());
        methodCall.setEnabled(true);
        methodCall.setMethodCallType(methodCallType.getType());
        methodCall.setCallerClassName(callerClassName);
        methodCall.setCallerMethodName(callerMethodName);
        methodCall.setCallerMethodArgTypes(callerMethodArgTypes);
        methodCall.setCallerSourceLine(JavaCG2Constants.DEFAULT_LINE_NUMBER);
        methodCall.setCallerReturnType(callerMethodReturnType);
        methodCall.setCalleeClassName(calleeClassName);
        methodCall.setCalleeMethodName(calleeMethodName);
        methodCall.setCalleeMethodArgTypes(calleeMethodArgTypes);
        methodCall.setRawReturnType(calleeMethodReturnType);
        JavaCG2FileUtil.write2FileWithTab(methodCallWriter, methodCall.genMethodCallContent(callerClassJarNum, calleeClassJarNum));
    }

    //
    public void setCallIdCounter(JavaCG2Counter callIdCounter) {
        this.callIdCounter = callIdCounter;
    }

    public void setInterfaceMethodWithArgTypesMap(Map<String, Map<MethodArgReturnTypes, Integer>> interfaceMethodWithArgTypesMap) {
        this.interfaceMethodWithArgTypesMap = interfaceMethodWithArgTypesMap;
    }

    public void setChildrenClassMap(Map<String, List<String>> childrenClassMap) {
        this.childrenClassMap = childrenClassMap;
    }

    public void setChildrenInterfaceMap(Map<String, List<String>> childrenInterfaceMap) {
        this.childrenInterfaceMap = childrenInterfaceMap;
    }

    public void setAllClassNameSet(Set<String> allClassNameSet) {
        this.allClassNameSet = allClassNameSet;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }

    public void setClassExtendsImplMethodWithArgTypesMap(Map<String, Map<MethodArgReturnTypes, Integer>> classExtendsImplMethodWithArgTypesMap) {
        this.classExtendsImplMethodWithArgTypesMap = classExtendsImplMethodWithArgTypesMap;
    }

    public void setInterfaceExtendsInfoMap(Map<String, List<String>> interfaceExtendsInfoMap) {
        this.interfaceExtendsInfoMap = interfaceExtendsInfoMap;
    }

    public void setClassImplementsInfoMap(Map<String, List<String>> classImplementsInfoMap) {
        this.classImplementsInfoMap = classImplementsInfoMap;
    }

    public void setClassExtendsInfoMap(Map<String, ClassExtendsInfo> classExtendsInfoMap) {
        this.classExtendsInfoMap = classExtendsInfoMap;
    }

    public void setMethodCallWriter(Writer methodCallWriter) {
        this.methodCallWriter = methodCallWriter;
    }
}
