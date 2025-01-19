package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.TypeConstants;
import com.adrninistrator.javacg2.common.enums.JavaCG2ConstantTypeEnum;
import com.adrninistrator.javacg2.dto.classes.InnerClassInfo;
import com.adrninistrator.javacg2.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg2.dto.type.JavaCG2Type;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.AccessFlags;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.InnerClass;
import org.apache.bcel.classfile.InnerClasses;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.LineNumberTable;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.classfile.Signature;
import org.apache.bcel.classfile.Utility;
import org.apache.bcel.generic.ArrayType;
import org.apache.bcel.generic.BasicType;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.Type;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/9/21
 * @description:
 */
public class JavaCG2ByteCodeUtil {

    private static final Logger logger = LoggerFactory.getLogger(JavaCG2ByteCodeUtil.class);

    /**
     * 判断当前方法是否为涉及继承的方法
     *
     * @param methodName
     * @param accessFlags
     * @return
     */
    public static boolean checkExtendsMethod(String methodName, AccessFlags accessFlags) {
       /*
            同时满足以下条件：
            非<init>、<clinit>
            非静态
            抽象方法，或public方法，或protected方法，或非public且非protected且非private方法
         */
        return !methodName.startsWith("<")
                && !accessFlags.isStatic()
                && (accessFlags.isAbstract() || accessFlags.isPublic() || accessFlags.isProtected() || !accessFlags.isPrivate());
    }

    /**
     * 判断当前方法是否为涉及实现的方法
     *
     * @param methodName
     * @param accessFlags
     * @return
     */
    public static boolean checkImplMethod(String methodName, AccessFlags accessFlags) {
       /*
            同时满足以下条件：
            非<init>、<clinit>
            非静态
            非抽象
            public方法（只能为public，不能为默认修饰符）
         */
        return !methodName.startsWith("<")
                && !accessFlags.isStatic()
                && !accessFlags.isAbstract()
                && accessFlags.isPublic();
    }

    /**
     * 获取可能涉及继承的相关方法，包含参数
     *
     * @param methods
     * @return
     */
    public static Map<MethodArgReturnTypes, Integer> genExtendsClassMethodWithArgTypes(Method[] methods) {
        Map<MethodArgReturnTypes, Integer> methodArgReturnTypesMap = new HashMap<>();
        // 遍历类的方法
        for (Method method : methods) {
            String methodName = method.getName();
            if (JavaCG2ByteCodeUtil.checkExtendsMethod(methodName, method)) {
                // 对于可能涉及继承的方法进行记录
                MethodArgReturnTypes methodArgReturnTypes = new MethodArgReturnTypes(methodName, method.getArgumentTypes(), method.getReturnType());
                methodArgReturnTypesMap.put(methodArgReturnTypes, method.getAccessFlags());
            }
        }
        return methodArgReturnTypesMap;
    }

    /**
     * 获取可能涉及实现的相关方法，包含参数
     *
     * @param methods
     * @return
     */
    public static Map<MethodArgReturnTypes, Integer> genImplClassMethodWithArgTypes(Method[] methods) {
        Map<MethodArgReturnTypes, Integer> methodInfoMap = new HashMap<>(methods.length);
        for (Method method : methods) {
            String methodName = method.getName();
            if (checkImplMethod(methodName, method)) {
                // 记录可能涉及实现的方法
                methodInfoMap.put(new MethodArgReturnTypes(methodName, method.getArgumentTypes(), method.getReturnType()), method.getAccessFlags());
            }
        }
        return methodInfoMap;
    }

    /**
     * 获取接口相关的方法，包含参数
     *
     * @param methods
     * @return
     */
    public static Map<MethodArgReturnTypes, Integer> genInterfaceMethodWithArgTypes(Method[] methods) {
        Map<MethodArgReturnTypes, Integer> methodInfoMap = new HashMap<>(methods.length);
        for (Method method : methods) {
            methodInfoMap.put(new MethodArgReturnTypes(method.getName(), method.getArgumentTypes(), method.getReturnType()), method.getAccessFlags());
        }
        return methodInfoMap;
    }

    /**
     * 获取方法的起始代码行号
     *
     * @param method
     * @return
     */
    public static int getFuncStartSourceLine(Method method) {
        LineNumberTable lineNumberTable = method.getLineNumberTable();
        if (lineNumberTable == null || lineNumberTable.getLineNumberTable() == null) {
            return JavaCG2Constants.DEFAULT_LINE_NUMBER;
        }

        return lineNumberTable.getLineNumberTable()[0].getLineNumber();
    }

    /**
     * 获取构造函数的起始代码行号
     *
     * @param javaClass
     * @return
     */
    public static int getInitFuncStartSourceLine(JavaClass javaClass) {
        Method[] methods = javaClass.getMethods();
        if (methods == null) {
            return JavaCG2Constants.DEFAULT_LINE_NUMBER;
        }

        for (Method method : methods) {
            if (JavaCG2ClassMethodUtil.isInitMethod(method.getName())) {
                return getFuncStartSourceLine(method);
            }
        }

        return JavaCG2Constants.DEFAULT_LINE_NUMBER;
    }

    /**
     * 去除类名中的数组形式
     *
     * @param className
     * @return
     */
    public static String removeArrayInClassName(String className) {
        if (!className.startsWith("[")) {
            return className;
        }

        // 处理数组格式
        String tmpClassName = Utility.typeSignatureToString(className, false);
        return removeOneArrayFlag(tmpClassName);
    }

    /**
     * 为类型增加数组类型
     *
     * @param className
     * @param arrayDimensions
     * @return
     */
    public static String addArrayFlag(String className, int arrayDimensions) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(className);
        for (int i = 0; i < arrayDimensions; i++) {
            stringBuilder.append(JavaCG2Constants.FLAG_ARRAY);
        }
        return stringBuilder.toString();
    }

    /**
     * 去掉数组形式最后的一个[]
     *
     * @param arrayType
     * @return
     */
    public static String removeOneArrayFlag(String arrayType) {
        if (arrayType == null) {
            return null;
        }

        if (!isArrayType(arrayType) && !isNullType(arrayType)) {
            logger.error("类名不是数组形式 {}", arrayType);
            return arrayType;
        }

        return arrayType.substring(0, arrayType.length() - JavaCG2Constants.FLAG_ARRAY.length());
    }

    /**
     * 去掉数组形式中全部的[]
     *
     * @param type
     * @return
     */
    public static String removeAllArrayFlag(String type) {
        return StringUtils.replace(type, JavaCG2Constants.FLAG_ARRAY, "");
    }

    /**
     * 判断是否为数组类型
     *
     * @param type
     * @return
     */
    public static boolean isArrayType(String type) {
        return type.endsWith(JavaCG2Constants.FLAG_ARRAY);
    }

    /**
     * 根据类型获取数组维度
     *
     * @param type
     * @return
     */
    public static int getTypeArrayDimensions(String type) {
        return StringUtils.countMatches(type, JavaCG2Constants.FLAG_ARRAY);
    }

    /**
     * 获取指定类型的长度
     *
     * @param typeString
     * @return
     */
    public static int getTypeSize(String typeString) {
        if (JavaCG2ConstantTypeEnum.CONSTTE_DOUBLE.getType().equals(typeString) || JavaCG2ConstantTypeEnum.CONSTTE_LONG.getType().equals(typeString)) {
            return 2;
        }
        return 1;
    }

    /**
     * 判断是否为与int兼容的类型
     *
     * @param type
     * @return false: 不类型 true: 类型
     */
    public static boolean checkCompatibleWithInt(String type) {
        for (String compatibleType : TypeConstants.COMPATIBLE_INT_TYPES) {
            if (compatibleType.equals(type)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 检查实际的类型与预期的是否相符
     *
     * @param actualType
     * @param expectedType
     * @return true: 相符 false: 不相符
     */
    public static boolean checkTypeString(String actualType, String expectedType) {
        if (JavaCG2ByteCodeUtil.isNullType(actualType)) {
            return true;
        }

        if (JavaCG2ByteCodeUtil.compareType(actualType, expectedType)) {
            return true;
        }

        logger.error("类型与预期的不一致 actualType: {} expectedType: {}", actualType, expectedType);
        return false;
    }

    /**
     * 判断类型是否为null
     *
     * @param type
     * @return false: 不为null true: 为null
     */
    public static boolean isNullType(String type) {
        return type == null || JavaCG2ConstantTypeEnum.CONSTTE_NULL.getType().equals(type);
    }

    /**
     * 比较两个类型是否兼容
     *
     * @param type1 不允许为null
     * @param type2
     * @return false: 不兼容 true: 兼容
     */
    public static boolean compareType(String type1, String type2) {
        if (type1.equals(type2)) {
            return true;
        }

        if (compareIntType(type1, type2)) {
            return true;
        }

        return compareByteBooleanType(type1, type2);
    }

    /**
     * 比较指定的两个类型是否为与int兼容的类型
     *
     * @param type1
     * @param type2
     * @return false: 与int不兼容 true: 与int兼容
     */
    public static boolean compareIntType(String type1, String type2) {
        if (JavaCG2ConstantTypeEnum.CONSTTE_INT.getType().equals(type1) && checkCompatibleWithInt(type2)) {
            return true;
        }

        return JavaCG2ConstantTypeEnum.CONSTTE_INT.getType().equals(type2) && checkCompatibleWithInt(type1);
    }

    /**
     * 比较指定的两个类型是否为byte、boolean（或一维数组）兼容的类型
     *
     * @param type1
     * @param type2
     * @return false: 不兼容 true: 兼容
     */
    public static boolean compareByteBooleanType(String type1, String type2) {
        if (JavaCG2ConstantTypeEnum.CONSTTE_BYTE.getType().equals(type1) && JavaCG2ConstantTypeEnum.CONSTTE_BOOLEAN.getType().equals(type2)) {
            return true;
        }

        if (JavaCG2ConstantTypeEnum.CONSTTE_BYTE.getType().equals(type2) && JavaCG2ConstantTypeEnum.CONSTTE_BOOLEAN.getType().equals(type1)) {
            return true;
        }

        if (TypeConstants.BYTE_ARRAY_TYPE.equals(type1) && TypeConstants.BOOLEAN_ARRAY_TYPE.equals(type2)) {
            return true;
        }

        return TypeConstants.BYTE_ARRAY_TYPE.equals(type2) && TypeConstants.BOOLEAN_ARRAY_TYPE.equals(type1);
    }

    /**
     * 判断是否为public
     *
     * @param accessFlags
     * @return
     */
    public static boolean isPublicFlag(int accessFlags) {
        return (accessFlags & Const.ACC_PUBLIC) != 0;
    }

    /**
     * 判断是否为protected
     *
     * @param accessFlags
     * @return
     */
    public static boolean isProtectedMethod(int accessFlags) {
        return (accessFlags & Const.ACC_PROTECTED) != 0;
    }

    /**
     * 判断是否为private
     *
     * @param accessFlags
     * @return
     */
    public static boolean isPrivateMethod(int accessFlags) {
        return (accessFlags & Const.ACC_PRIVATE) != 0;
    }

    /**
     * 判断是否为abstract
     *
     * @param accessFlags
     * @return
     */
    public static boolean isAbstractFlag(int accessFlags) {
        return (accessFlags & Const.ACC_ABSTRACT) != 0;
    }

    /**
     * 判断是否为interface
     *
     * @param accessFlags
     * @return
     */
    public static boolean isInterfaceFlag(int accessFlags) {
        return (accessFlags & Const.ACC_INTERFACE) != 0;
    }

    /**
     * 判断是否有ACC_BRIDGE标志
     *
     * @param accessFlags
     * @return
     */
    public static boolean isBridgeFlag(int accessFlags) {
        return (accessFlags & Const.ACC_BRIDGE) != 0;
    }

    /**
     * 判断是否有ACC_SYNTHETIC标志
     *
     * @param accessFlags
     * @return
     */
    public static boolean isSyntheticFlag(int accessFlags) {
        return (accessFlags & Const.ACC_SYNTHETIC) != 0;
    }

    /**
     * 判断是否为Enum
     *
     * @param accessFlags
     * @return
     */
    public static boolean isEnumFlag(int accessFlags) {
        return (accessFlags & Const.ACC_ENUM) != 0;
    }

    /**
     * 设置public标志
     *
     * @param accessFlags
     * @param trueOrFalse
     * @return
     */
    public static int setPublicFlag(int accessFlags, boolean trueOrFalse) {
        return setFlag(accessFlags, Const.ACC_PUBLIC, trueOrFalse);
    }

    /**
     * 设置protected标志
     *
     * @param accessFlags
     * @param trueOrFalse
     * @return
     */
    public static int setProtectedFlag(int accessFlags, boolean trueOrFalse) {
        return setFlag(accessFlags, Const.ACC_PROTECTED, trueOrFalse);
    }

    /**
     * 设置public标志
     *
     * @param accessFlags
     * @param trueOrFalse
     * @return
     */
    public static int setAbstractFlag(int accessFlags, boolean trueOrFalse) {
        return setFlag(accessFlags, Const.ACC_ABSTRACT, trueOrFalse);
    }

    /**
     * 设置指定位的标记
     *
     * @param accessFlags
     * @param flag
     * @param trueOrFalse
     * @return
     */
    private static int setFlag(int accessFlags, int flag, boolean trueOrFalse) {
        if ((accessFlags & flag) != 0) { // Flag is set already
            if (!trueOrFalse) {
                accessFlags ^= flag;
            }
        } else if (trueOrFalse) {
            accessFlags |= flag;
        }
        return accessFlags;
    }

    /**
     * 获得修饰符字符串
     *
     * @param accessFlags
     * @return
     */
    public static String getModifiersString(int accessFlags) {
        if (isPublicFlag(accessFlags)) {
            return JavaCG2CommonNameConstants.MODIFIERS_PUBLIC;
        }
        if (isProtectedMethod(accessFlags)) {
            return JavaCG2CommonNameConstants.MODIFIERS_PROTECTED;
        }
        if (isPrivateMethod(accessFlags)) {
            return JavaCG2CommonNameConstants.MODIFIERS_PRIVATE;
        }
        return JavaCG2CommonNameConstants.MODIFIERS_DEFAULT;
    }

    /**
     * 获取类的Signature
     *
     * @param javaClass
     * @return
     */
    public static Signature getSignatureOfClass(JavaClass javaClass) {
        for (Attribute attribute : javaClass.getAttributes()) {
            if (attribute instanceof Signature) {
                return (Signature) attribute;
            }
        }
        return null;
    }

    /**
     * 判断指定的类是否为匿名内部类
     *
     * @param javaClass
     * @return
     */
    public static boolean checkInnerAnonymousClass(JavaClass javaClass) {
        // 根据类名判断是否为匿名内部类
        if (!JavaCG2ClassMethodUtil.isInnerAnonymousClass(javaClass.getClassName())) {
            return false;
        }

        for (Attribute attribute : javaClass.getAttributes()) {
            if (!(attribute instanceof InnerClasses)) {
                continue;
            }
            InnerClasses innerClasses = (InnerClasses) attribute;
            for (InnerClass innerClass : innerClasses.getInnerClasses()) {
                int innerClassIndex = innerClass.getInnerClassIndex();
                if (innerClassIndex == javaClass.getClassNameIndex()) {
                    // 当前类是内部类
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * 获取类中的内部类信息
     *
     * @param javaClass
     * @return
     */
    public static List<InnerClassInfo> getInnerClassInfo(JavaClass javaClass) {
        List<InnerClassInfo> innerClassInfoList = new ArrayList<>(0);
        String className = javaClass.getClassName();
        ConstantPool constantPool = javaClass.getConstantPool();
        for (Attribute attribute : javaClass.getAttributes()) {
            if (!(attribute instanceof InnerClasses)) {
                continue;
            }
            InnerClasses innerClasses = (InnerClasses) attribute;
            for (InnerClass innerClass : innerClasses.getInnerClasses()) {
                int innerClassIndex = innerClass.getInnerClassIndex();
                if (innerClassIndex == javaClass.getClassNameIndex()) {
                    // 当前类是内部类，跳过
                    continue;
                }

                String innerClassName = constantPool.getConstantString(innerClassIndex, Const.CONSTANT_Class);
                innerClassName = Utility.compactClassName(innerClassName, false);
                if (JavaCG2ClassMethodUtil.isClassInJdk(innerClassName) ||
                        !innerClassName.startsWith(className) ||
                        StringUtils.countMatches(innerClassName, '$') < StringUtils.countMatches(className, '$')) {
                    /*
                        JDK中的类，跳过
                        当前内部类不是当前类的内部类，跳过
                        当前类是当前内部类对应的外部类，跳过
                     */
                    continue;
                }

                InnerClassInfo innerClassInfo = new InnerClassInfo(innerClassName, className, JavaCG2ClassMethodUtil.isInnerAnonymousClass(innerClassName));
                innerClassInfoList.add(innerClassInfo);
            }
        }
        return innerClassInfoList;
    }

    /**
     * 获取指定的变量序号在当前方法中的参数序号
     *
     * @param mg
     * @param variableIndex 变量序号
     * @return -1: 不是获取方法参数 >=0: 方法参数序号（从0开始）
     */
    public static int getArgIndexInMethod(MethodGen mg, int variableIndex) {
        int argIndex = variableIndex - (mg.isStatic() ? 0 : 1);
        return argIndex < mg.getArgumentTypes().length ? argIndex : -1;
    }

    /**
     * 获取指定的方法参数在LocalVariableTable中的序号
     *
     * @param mg
     * @param argIndex
     * @return
     */
    public static int getLocalVariableTableIndex(MethodGen mg, int argIndex) {
        return argIndex + (mg.isStatic() ? 0 : 1);
    }

    /**
     * 获得字段类型，以及属于数组类型时的数组维度
     *
     * @param type
     * @return
     */
    public static JavaCG2Type genJavaCG2Type(Type type) {
        JavaCG2Type javaCG2Type = new JavaCG2Type();
        String typeStr;
        int arrayDimensions = 0;
        if (type instanceof BasicType) {
            // 字段类型为基本类型
            BasicType basicType = (BasicType) type;
            typeStr = basicType.getClassName();
        } else if (type instanceof ObjectType) {
            // 字段类型为类类型
            ObjectType objectType = (ObjectType) type;
            typeStr = objectType.getClassName();
        } else if (type instanceof ArrayType) {
            // 字段类型为数组
            ArrayType arrayType = (ArrayType) type;
            arrayDimensions = arrayType.getDimensions();
            typeStr = arrayType.getBasicType().toString();
        } else {
            typeStr = type.toString();
        }
        javaCG2Type.setType(typeStr);
        javaCG2Type.setArrayDimensions(arrayDimensions);
        return javaCG2Type;
    }

    /**
     * 生成文件中多列形式的泛型信息字符串，用于固定类型
     *
     * @param javaCG2Type
     * @return
     */
    public static String genGenericsTypeStr4Fixed(JavaCG2Type javaCG2Type) {
        return JavaCG2FileUtil.appendFileColumn("0", javaCG2Type.getType(), String.valueOf(javaCG2Type.getArrayDimensions()), "", "", "", "");
    }

    /**
     * 获取源代码行号
     *
     * @param instructionPosition
     * @param lineNumberTable
     * @return
     */
    public static int getSourceLine(int instructionPosition, LineNumberTable lineNumberTable) {
        if (lineNumberTable == null) {
            return JavaCG2Constants.DEFAULT_LINE_NUMBER;
        }
        int sourceLine = lineNumberTable.getSourceLine(instructionPosition);
        return Math.max(sourceLine, 0);
    }

    private JavaCG2ByteCodeUtil() {
        throw new IllegalStateException("illegal");
    }
}
