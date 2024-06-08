package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGConstantTypeEnum;
import com.adrninistrator.javacg.dto.classes.ClassExtendsMethodInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.interfaces.InterfaceExtendsMethodInfo;
import com.adrninistrator.javacg.dto.method.JavaCGMethodInfo;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: 方法处理相关工具类
 */
public class JavaCGClassMethodUtil {

    /**
     * 生成格式化后的完整方法
     *
     * @param javaCGMethodInfo 方法信息
     * @return
     */
    public static String formatFullMethod(JavaCGMethodInfo javaCGMethodInfo) {
        return formatFullMethod(javaCGMethodInfo.getClassName(), javaCGMethodInfo.getMethodName(), javaCGMethodInfo.getMethodArgumentTypes());
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
        return formatFullMethod(className, methodName, JavaCGConstants.EMPTY_METHOD_ARGS);
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
        return className + JavaCGConstants.FLAG_COLON + methodName + argTypes;
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
        StringBuilder sb = new StringBuilder(JavaCGConstants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCGConstants.FLAG_COMMA);
            }
            sb.append(argTypes[i].toString());
        }
        sb.append(JavaCGConstants.FLAG_RIGHT_BRACKET);
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
        return className + JavaCGConstants.FLAG_COLON + methodNameAndArgs;
    }

    /**
     * 生成格式化后的方法名+方法参数
     *
     * @param methodName 方法名，不包含()
     * @param argTypes   方法参数类型
     * @return
     */
    public static String formatMethodWithArgTypes(String methodName, Class<?>... argTypes) {
        StringBuilder sb = new StringBuilder(methodName).append(JavaCGConstants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCGConstants.FLAG_COMMA);
            }
            sb.append(argTypes[i].getName());
        }
        sb.append(JavaCGConstants.FLAG_RIGHT_BRACKET);
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
            return methodName + JavaCGConstants.EMPTY_METHOD_ARGS;
        }

        StringBuilder sb = new StringBuilder(methodName).append(JavaCGConstants.FLAG_LEFT_BRACKET);
        for (int i = 0; i < argTypes.length; i++) {
            if (i != 0) {
                sb.append(JavaCGConstants.FLAG_COMMA);
            }
            sb.append(argTypes[i]);
        }
        sb.append(JavaCGConstants.FLAG_RIGHT_BRACKET);
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
        return className + JavaCGConstants.SEPARATOR_CLASS_FIELD + fieldName;
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
        return JavaCGUtil.getSubStringAfterLast(className, JavaCGConstants.FLAG_DOT);
    }

    /**
     * 判断childClassName是否直接或间接继承自superClassName
     *
     * @param childClassName            子类类名
     * @param superClassName            超类类名
     * @param classExtendsMethodInfoMap 类涉及继承的信息
     * @return
     */
    public static boolean isChildOf(String childClassName, String superClassName, Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap) {
        if (childClassName == null || superClassName == null || classExtendsMethodInfoMap == null) {
            throw new JavaCGRuntimeException("传入参数不允许为空");
        }

        String currentClassName = childClassName;
        while (true) {
            ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoMap.get(currentClassName);
            if (classExtendsMethodInfo == null) {
                // 找不到当前类的父类信息
                return false;
            }

            if (superClassName.equals(classExtendsMethodInfo.getSuperClassName())) {
                // 当前类的父类是指定的父类
                return true;
            }

            // 继续处理父类
            currentClassName = classExtendsMethodInfo.getSuperClassName();
        }
    }

    /**
     * 判断childClassName是否直接或间接实现了interfaceName
     *
     * @param className                     类名
     * @param interfaceName                 接口名
     * @param classExtendsMethodInfoMap     类涉及继承的信息
     * @param classImplementsMethodInfoMap  类实现的接口信息
     * @param interfaceExtendsMethodInfoMap 接口涉及继承的信息
     * @return
     */
    public static boolean isImplementationOf(String className,
                                             String interfaceName,
                                             Map<String, ClassExtendsMethodInfo> classExtendsMethodInfoMap,
                                             Map<String, ClassImplementsMethodInfo> classImplementsMethodInfoMap,
                                             Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap
    ) {
        if (className == null || interfaceName == null || classExtendsMethodInfoMap == null || classImplementsMethodInfoMap == null || interfaceExtendsMethodInfoMap == null) {
            throw new JavaCGRuntimeException("传入参数不允许为空");
        }

        String currentClassName = className;
        while (true) {
            ClassImplementsMethodInfo classImplementsMethodInfo = classImplementsMethodInfoMap.get(currentClassName);
            if (classImplementsMethodInfo != null) {
                List<String> interfaceNameList = classImplementsMethodInfo.getInterfaceNameList();
                if (interfaceNameList != null) {
                    if (interfaceNameList.contains(interfaceName)) {
                        // 当前类实现的接口中包含指定的接口
                        return true;
                    }

                    for (String currentInterfaceName : interfaceNameList) {
                        if (isSuperInterfaceOf(currentInterfaceName, interfaceName, interfaceExtendsMethodInfoMap)) {
                            // 当前类实现的接口继承了指定的接口
                            return true;
                        }
                    }
                }
            }

            ClassExtendsMethodInfo classExtendsMethodInfo = classExtendsMethodInfoMap.get(currentClassName);
            if (classExtendsMethodInfo == null) {
                // 找不到当前类实现的接口信息
                return false;
            }

            // 继续处理父类
            currentClassName = classExtendsMethodInfo.getSuperClassName();
        }
    }

    /**
     * @param childInterfaceName            子类接口名
     * @param superInterfaceName            超类接口名
     * @param interfaceExtendsMethodInfoMap 接口涉及继承的信息
     * @return
     */
    public static boolean isSuperInterfaceOf(String childInterfaceName, String superInterfaceName, Map<String, InterfaceExtendsMethodInfo> interfaceExtendsMethodInfoMap) {
        if (childInterfaceName == null || superInterfaceName == null || interfaceExtendsMethodInfoMap == null) {
            throw new JavaCGRuntimeException("传入参数不允许为空");
        }

        while (true) {
            InterfaceExtendsMethodInfo interfaceExtendsMethodInfo = interfaceExtendsMethodInfoMap.get(childInterfaceName);
            if (interfaceExtendsMethodInfo == null) {
                // 找不到当前接口继承的接口信息
                return false;
            }

            List<String> superInterfaceList = interfaceExtendsMethodInfo.getSuperInterfaceList();
            if (superInterfaceList.isEmpty()) {
                // 找不到当前接口继承的接口信息
                return false;
            }

            if (superInterfaceList.contains(superInterfaceName)) {
                // 当前接口继承的接口包含指定接口
                return true;
            }

            // 处理当前接口继承的接口，递归调用
            for (String currentSuperInterfaceName : superInterfaceList) {
                if (isSuperInterfaceOf(currentSuperInterfaceName, superInterfaceName, interfaceExtendsMethodInfoMap)) {
                    return true;
                }
            }
            // 当前接口继承的接口也没有继承指定的超类接口
            return false;
        }
    }

    /**
     * 判断是否为<init>方法
     *
     * @param methodName
     * @return
     */
    public static boolean isInitMethod(String methodName) {
        return JavaCGCommonNameConstants.METHOD_NAME_INIT.equals(methodName);
    }

    /**
     * 判断是否为Object类
     *
     * @param className
     * @return
     */
    public static boolean isObjectClass(String className) {
        return JavaCGCommonNameConstants.CLASS_NAME_OBJECT.equals(className);
    }

    /**
     * 判断指定类是否为JDK中的类
     *
     * @param className
     * @return
     */
    public static boolean isClassInJdk(String className) {
        return StringUtils.startsWith(className, JavaCGCommonNameConstants.PACKAGE_JAVA);
    }

    /**
     * 判断是否为自定义类型，非JDK中的类，也非基本类型
     *
     * @param type
     * @return
     */
    public static boolean isCustomType(String type) {
        // 去掉数组形式中全部的[]
        String typeWithoutArray = JavaCGByteCodeUtil.removeAllArrayFlag(type);
        return !isClassInJdk(typeWithoutArray) && !JavaCGConstantTypeEnum.isConstantType(typeWithoutArray);
    }

    /**
     * 获取类的包名
     *
     * @param className
     * @return
     */
    public static String getPackageName(String className) {
        return StringUtils.substringBeforeLast(className, JavaCGConstants.FLAG_DOT);
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
        return StringUtils.contains(getSimpleClassNameFromFull(className), JavaCGConstants.FLAG_DOLOR);
    }

    /**
     * 判断类名是否为匿名内部类
     *
     * @param className
     * @return
     */
    public static boolean isInnerAnonymousClass(String className) {
        String tail = StringUtils.substringAfterLast(className, JavaCGConstants.FLAG_DOLOR);
        return JavaCGUtil.isNumStr(tail);
    }

    /**
     * 判断方法名称是否以get开头
     *
     * @param methodName
     * @return
     */
    public static boolean matchesGetMethod(String methodName) {
        return methodName.startsWith(JavaCGConstants.METHOD_PREFIX_GET);
    }

    /**
     * 判断方法名称是否以set开头
     *
     * @param methodName
     * @return
     */
    public static boolean matchesSetMethod(String methodName) {
        return methodName.startsWith(JavaCGConstants.METHOD_PREFIX_SET);
    }

    private JavaCGClassMethodUtil() {
        throw new IllegalStateException("illegal");
    }
}
