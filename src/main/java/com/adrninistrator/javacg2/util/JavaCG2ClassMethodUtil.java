package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.dto.classes.ClassExtendsInfo;
import com.adrninistrator.javacg2.dto.method.JavaCG2MethodInfo;
import com.adrninistrator.javacg2.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: 方法处理相关工具类
 */
public class JavaCG2ClassMethodUtil {

    /**
     * 生成格式化后的完整方法
     *
     * @param javaCG2MethodInfo 方法信息
     * @return
     */
    public static String formatFullMethod(JavaCG2MethodInfo javaCG2MethodInfo) {
        return formatFullMethod(javaCG2MethodInfo.getClassName(), javaCG2MethodInfo.getMethodName(), javaCG2MethodInfo.getMethodArgumentTypes());
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param mg 方法
     * @return
     */
    public static String formatFullMethod(MethodGen mg) {
        return formatFullMethod(mg.getClassName(), mg.getName(), mg.getArgumentTypes());
    }

    /**
     * 生成格式化后的完整方法，方法参数为空
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @return
     */
    public static String formatFullMethodNoArgs(String className, String methodName) {
        return formatFullMethod(className, methodName, JavaCG2Constants.EMPTY_METHOD_ARGS);
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型，包含起始的()，参数类名之间需要使用半角逗号,分隔，不能包含空格，参数类名也需要为完整类名
     * @return
     */
    public static String formatFullMethod(String className, String methodName, Type[] argTypes) {
        return formatFullMethod(className, methodName, getArgTypeStr(argTypes));
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型，包含起始的()，参数类名之间需要使用半角逗号,分隔，不能包含空格，参数类名也需要为完整类名
     * @return
     */
    public static String formatFullMethod(String className, String methodName, String argTypes) {
        return className + JavaCG2Constants.FLAG_COLON + methodName + argTypes;
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatFullMethod(String className, String methodName, Class<?>... argTypes) {
        return formatFullMethodWithArgTypes(className, formatMethodWithArgTypes(methodName, argTypes));
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className  完整类名
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatFullMethodStr(String className, String methodName, String... argTypes) {
        return formatFullMethodWithArgTypes(className, formatMethodWithArgTypesStr(methodName, argTypes));
    }

    /**
     * 生成参数类型的字符串形式
     *
     * @param argTypes
     * @return
     */
    public static String getArgTypeStr(Type[] argTypes) {
        StringBuilder sb = new StringBuilder(JavaCG2Constants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCG2Constants.FLAG_COMMA);
            }
            sb.append(argTypes[i].toString());
        }
        sb.append(JavaCG2Constants.FLAG_RIGHT_BRACKET);
        return sb.toString();
    }

    /**
     * 生成格式化后的完整方法
     *
     * @param className         完整类名
     * @param methodNameAndArgs 方法名+方法参数
     * @return
     */
    public static String formatFullMethodWithArgTypes(String className, String methodNameAndArgs) {
        return className + JavaCG2Constants.FLAG_COLON + methodNameAndArgs;
    }

    /**
     * 生成格式化后的方法名+方法参数
     *
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatMethodWithArgTypes(String methodName, Class<?>... argTypes) {
        StringBuilder sb = new StringBuilder(methodName).append(JavaCG2Constants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCG2Constants.FLAG_COMMA);
            }
            sb.append(argTypes[i].getName());
        }
        sb.append(JavaCG2Constants.FLAG_RIGHT_BRACKET);
        return sb.toString();
    }

    /**
     * 生成格式化后的方法名+方法参数
     *
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatMethodWithArgTypesStr(String methodName, String... argTypes) {
        if (ArrayUtils.isEmpty(argTypes)) {
            return methodName + JavaCG2Constants.EMPTY_METHOD_ARGS;
        }

        StringBuilder sb = new StringBuilder(methodName).append(JavaCG2Constants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCG2Constants.FLAG_COMMA);
            }
            sb.append(argTypes[i]);
        }
        sb.append(JavaCG2Constants.FLAG_RIGHT_BRACKET);
        return sb.toString();
    }

    /**
     * 拼接类名与字段名
     *
     * @param className
     * @param fieldName
     * @return
     */
    public static String genClassAndField(String className, String fieldName) {
        return className + JavaCG2Constants.SEPARATOR_CLASS_FIELD + fieldName;
    }

    /**
     * 获取简单类名首字母小写后的结果
     *
     * @param simpleClassName 简单类名
     * @return
     */
    public static String getFirstLetterLowerClassName(String simpleClassName) {
        if (simpleClassName == null) {
            return null;
        }
        if (simpleClassName.isEmpty()) {
            return "";
        }
        String firstLetterLower = simpleClassName.substring(0, 1).toLowerCase();
        if (simpleClassName.length() == 1) {
            return firstLetterLower;
        }

        return firstLetterLower + simpleClassName.substring(1);
    }

    /**
     * 从完整类名中获取简单类名（去掉包名）
     *
     * @param className 完整类名
     * @return
     */
    public static String getSimpleClassNameFromFull(String className) {
        return JavaCG2Util.getSubStringAfterLast(className, JavaCG2Constants.FLAG_DOT);
    }

    /**
     * 从完整方法信息中获取方法参数类型，不包含括号
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodArgTypes(String fullMethod) {
        return StringUtils.substringBetween(fullMethod, JavaCG2Constants.FLAG_LEFT_BRACKET, JavaCG2Constants.FLAG_RIGHT_BRACKET);
    }

    /**
     * 获得方法参数数量
     *
     * @param fullMethod
     * @return
     */
    public static int getMethodArgNum(String fullMethod) {
        String[] argTypes = StringUtils.splitPreserveAllTokens(getMethodArgTypes(fullMethod), JavaCG2Constants.FLAG_COMMA);
        return argTypes.length;
    }

    /**
     * 判断childClassName是否直接或间接继承自superClassName
     *
     * @param childClassName      子类类名
     * @param superClassName      超类类名
     * @param classExtendsInfoMap 类涉及继承的信息
     * @return
     */
    public static boolean isChildOf(String childClassName, String superClassName, Map<String, ClassExtendsInfo> classExtendsInfoMap) {
        if (childClassName == null || superClassName == null || classExtendsInfoMap == null) {
            throw new JavaCG2RuntimeException("传入参数不允许为空");
        }

        String currentClassName = childClassName;
        while (true) {
            ClassExtendsInfo classExtendsInfo = classExtendsInfoMap.get(currentClassName);
            if (classExtendsInfo == null) {
                // 找不到当前类的父类信息
                return false;
            }

            if (superClassName.equals(classExtendsInfo.getSuperClassName())) {
                // 当前类的父类是指定的父类
                return true;
            }

            // 继续处理父类
            currentClassName = classExtendsInfo.getSuperClassName();
        }
    }

    /**
     * 判断childClassName是否直接或间接实现了interfaceName
     *
     * @param className               类名
     * @param interfaceName           接口名
     * @param classExtendsInfoMap     类涉及继承的信息
     * @param classImplementsInfoMap  类实现的接口信息
     * @param interfaceExtendsInfoMap 接口涉及继承的信息
     * @return
     */
    public static boolean isImplementationOf(String className,
                                             String interfaceName,
                                             Map<String, ClassExtendsInfo> classExtendsInfoMap,
                                             Map<String, List<String>> classImplementsInfoMap,
                                             Map<String, List<String>> interfaceExtendsInfoMap) {
        if (className == null || interfaceName == null || classExtendsInfoMap == null || classImplementsInfoMap == null || interfaceExtendsInfoMap == null) {
            throw new JavaCG2RuntimeException("传入参数不允许为空");
        }

        String currentClassName = className;
        while (true) {
            List<String> interfaceNameList = classImplementsInfoMap.get(currentClassName);
            if (interfaceNameList != null) {
                if (interfaceNameList.contains(interfaceName)) {
                    // 当前类实现的接口中包含指定的接口
                    return true;
                }

                for (String currentInterfaceName : interfaceNameList) {
                    if (isSuperInterfaceOf(currentInterfaceName, interfaceName, interfaceExtendsInfoMap)) {
                        // 当前类实现的接口继承了指定的接口
                        return true;
                    }
                }
            }

            ClassExtendsInfo classExtendsInfo = classExtendsInfoMap.get(currentClassName);
            if (classExtendsInfo == null) {
                // 找不到当前类实现的接口信息
                return false;
            }

            // 继续处理父类
            currentClassName = classExtendsInfo.getSuperClassName();
        }
    }

    /**
     * @param childInterfaceName      子类接口名
     * @param superInterfaceName      超类接口名
     * @param interfaceExtendsInfoMap 接口涉及继承的信息
     * @return
     */
    public static boolean isSuperInterfaceOf(String childInterfaceName, String superInterfaceName, Map<String, List<String>> interfaceExtendsInfoMap) {
        if (childInterfaceName == null || superInterfaceName == null || interfaceExtendsInfoMap == null) {
            throw new JavaCG2RuntimeException("传入参数不允许为空");
        }

        List<String> superInterfaceList = interfaceExtendsInfoMap.get(childInterfaceName);
        if (JavaCG2Util.isCollectionEmpty(superInterfaceList)) {
            // 找不到当前接口继承的接口信息
            return false;
        }

        if (superInterfaceList.contains(superInterfaceName)) {
            // 当前接口继承的接口包含指定接口
            return true;
        }

        // 处理当前接口继承的接口，递归调用
        for (String currentSuperInterfaceName : superInterfaceList) {
            if (isSuperInterfaceOf(currentSuperInterfaceName, superInterfaceName, interfaceExtendsInfoMap)) {
                return true;
            }
        }
        // 当前接口继承的接口也没有继承指定的超类接口
        return false;
    }

    /**
     * 判断是否为<init>方法
     *
     * @param methodName
     * @return
     */
    public static boolean isInitMethod(String methodName) {
        return JavaCG2CommonNameConstants.METHOD_NAME_INIT.equals(methodName);
    }

    /**
     * 判断是否为Object类
     *
     * @param className
     * @return
     */
    public static boolean isObjectClass(String className) {
        return JavaCG2CommonNameConstants.CLASS_NAME_OBJECT.equals(className);
    }

    /**
     * 判断指定类是否为JDK中的类
     *
     * @param className
     * @return
     */
    public static boolean isClassInJdk(String className) {
        return StringUtils.startsWithAny(className, JavaCG2CommonNameConstants.PACKAGES_JDK);
    }

    /**
     * 判断是否为自定义类型，非JDK中的类，也非基本类型
     *
     * @param type
     * @return
     */
    public static boolean isCustomType(String type) {
        // 去掉数组形式中全部的[]
        String typeWithoutArray = JavaCG2ByteCodeUtil.removeAllArrayFlag(type);
        return !isClassInJdk(typeWithoutArray) && !JavaCG2ConstantTypeEnum.isConstantType(typeWithoutArray);
    }

    /**
     * 获得类对应的类型，自定义类型，或JDK中的类
     *
     * @param className
     * @return
     */
    public static String getClassCategory(String className) {
        if (StringUtils.isBlank(className)) {
            return "";
        }
        return isCustomType(className) ? JavaCG2Constants.FILE_KEY_CATEGORY_CUSTOM : JavaCG2Constants.FILE_KEY_CATEGORY_JDK;
    }

    /**
     * 获取类的包名
     *
     * @param className
     * @return
     */
    public static String getPackageName(String className) {
        return StringUtils.substringBeforeLast(className, JavaCG2Constants.FLAG_DOT);
    }

    /**
     * 获取包名层级，若包名为空，则返回0
     *
     * @param packageName
     * @return
     */
    public static int getPackageLevel(String packageName) {
        if (StringUtils.isBlank(packageName)) {
            return 0;
        }
        return StringUtils.countMatches(packageName, JavaCG2Constants.FLAG_DOT) + 1;
    }

    /**
     * 判断指定的两个类的包名是否相同
     *
     * @param className1
     * @param className2
     * @return
     */
    public static boolean checkSamePackage(String className1, String className2) {
        return StringUtils.equals(getPackageName(className1), getPackageName(className2));
    }

    /**
     * 判断类名是否为内部类
     *
     * @param className
     * @return
     */
    public static boolean isInnerClass(String className) {
        return StringUtils.contains(getSimpleClassNameFromFull(className), JavaCG2Constants.FLAG_DOLOR);
    }

    /**
     * 判断类名是否为匿名内部类
     *
     * @param className
     * @return
     */
    public static boolean isInnerAnonymousClass(String className) {
        String tail = StringUtils.substringAfterLast(className, JavaCG2Constants.FLAG_DOLOR);
        return JavaCG2Util.isNumStr(tail);
    }

    /**
     * 判断方法名称是否以get开头
     *
     * @param methodName
     * @return
     */
    public static boolean matchesGetMethod(String methodName) {
        return StringUtils.startsWithAny(methodName, JavaCG2Constants.METHOD_PREFIX_GET, JavaCG2Constants.METHOD_PREFIX_IS);
    }

    /**
     * 判断方法名称是否以set开头
     *
     * @param methodName
     * @return
     */
    public static boolean matchesSetMethod(String methodName) {
        return methodName.startsWith(JavaCG2Constants.METHOD_PREFIX_SET);
    }

    /**
     * 拷贝类的方法信息到Map，避免直接覆盖Map
     *
     * @param classMethodMap
     * @param className
     * @param methodArgReturnTypesMap
     */
    public static void copyClassMethodMap(Map<String, Map<MethodArgReturnTypes, Integer>> classMethodMap, String className,
                                          Map<MethodArgReturnTypes, Integer> methodArgReturnTypesMap) {
        if (JavaCG2Util.isMapEmpty(methodArgReturnTypesMap)) {
            return;
        }

        Map<MethodArgReturnTypes, Integer> existedMethodMap = classMethodMap.computeIfAbsent(className, k -> new HashMap<>());
        for (Map.Entry<MethodArgReturnTypes, Integer> entry : methodArgReturnTypesMap.entrySet()) {
            existedMethodMap.putIfAbsent(entry.getKey(), entry.getValue());
        }
    }

    /**
     * 从完整方法信息中获取完整类名
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getClassNameFromMethod(String method) {
        return StringUtils.substringBeforeLast(method, JavaCG2Constants.FLAG_COLON);
    }

    /**
     * 从完整方法信息中获取简单类名（去掉包名）
     *
     * @param method 完整方法信息
     * @return
     */
    public static String getSimpleClassNameFromMethod(String method) {
        String className = getClassNameFromMethod(method);
        return JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
    }

    /**
     * 从完整方法信息中获取方法名
     *
     * @param fullMethod 完整方法信息
     * @return
     */
    public static String getMethodNameFromFull(String fullMethod) {
        return StringUtils.substringBetween(fullMethod, JavaCG2Constants.FLAG_COLON, JavaCG2Constants.FLAG_LEFT_BRACKET);
    }


    private JavaCG2ClassMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
