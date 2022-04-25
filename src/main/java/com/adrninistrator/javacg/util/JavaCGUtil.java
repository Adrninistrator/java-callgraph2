package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.ClassInterfaceMethodInfo;
import com.adrninistrator.javacg.dto.ExtendsClassMethodInfo;
import com.adrninistrator.javacg.dto.MethodInfo;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.Type;

import java.io.BufferedWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2021/6/22
 * @description:
 */

public class JavaCGUtil {

    private static boolean debugPrintFlag = System.getProperty(JavaCGConstants.PROPERTY_DEBUG_PRINT) != null;

    public static boolean isInnerAnonymousClass(String className) {
        if (!className.contains("$")) {
            return false;
        }

        String[] array = className.split("\\$");
        if (array.length != 2) {
            return false;
        }

        return isNumStr(array[1]);
    }

    public static boolean isNumStr(String str) {
        if (str == null || str.isEmpty()) {
            return false;
        }

        char[] charArray = str.toCharArray();
        for (char ch : charArray) {
            if (ch < '0' || ch > '9') {
                return false;
            }
        }
        return true;
    }

    public static String getArgListStr(Type[] arguments) {
        StringBuilder sb = new StringBuilder("(");
        for (int i = 0; i < arguments.length; i++) {
            if (i != 0) {
                sb.append(",");
            }
            sb.append(arguments[i].toString());
        }
        sb.append(")");
        return sb.toString();
    }

    public static List<String> genImplClassMethodWithArgs(Method[] methods) {
        List<String> methodInfoList = new ArrayList<>(methods.length);
        for (Method method : methods) {
            String methodName = method.getName();
            // 忽略"<init>"和"<clinit>"方法
            if (!methodName.startsWith("<") && method.isPublic() && !method.isAbstract() && !method.isStatic()) {
                methodInfoList.add(methodName + JavaCGUtil.getArgListStr(method.getArgumentTypes()));
            }
        }
        return methodInfoList;
    }

    public static List<String> genInterfaceAbstractMethodWithArgs(Method[] methods) {
        List<String> methodInfoList = new ArrayList<>(methods.length);
        for (Method method : methods) {
            if (method.isAbstract()) {
                methodInfoList.add(method.getName() + JavaCGUtil.getArgListStr(method.getArgumentTypes()));
            }
        }
        return methodInfoList;
    }

    public static String getLambdaOrigMethod(String lambdaMethod) {
        int indexLastLambda = lambdaMethod.lastIndexOf(JavaCGConstants.FLAG_LAMBDA);
        String tmpString = lambdaMethod.substring(indexLastLambda + JavaCGConstants.FLAG_LAMBDA.length());
        int indexDollar = tmpString.indexOf('$');
        return tmpString.substring(0, indexDollar);
    }

    public static int getFuncStartSourceLine(Method method) {
        LineNumberTable lineNumberTable = method.getLineNumberTable();
        if (lineNumberTable == null || lineNumberTable.getLineNumberTable() == null) {
            return JavaCGConstants.DEFAULT_LINE_NUMBER;
        }

        return lineNumberTable.getLineNumberTable()[0].getLineNumber();
    }

    private static int getInitFuncStartSourceLine(JavaClass javaClass) {
        Method[] methods = javaClass.getMethods();
        if (methods == null) {
            return JavaCGConstants.DEFAULT_LINE_NUMBER;
        }

        for (Method method : methods) {
            if (JavaCGConstants.METHOD_NAME_INIT.equals(method.getName())) {
                return JavaCGUtil.getFuncStartSourceLine(method);
            }
        }

        return JavaCGConstants.DEFAULT_LINE_NUMBER;
    }

    /**
     * 获得JavaClass中指定下标的BootstrapMethod
     *
     * @param javaClass
     * @param index
     * @return
     */
    public static BootstrapMethod getBootstrapMethod(JavaClass javaClass, int index) {
        Attribute[] attributes = javaClass.getAttributes();
        for (Attribute attribute : attributes) {
            if (attribute instanceof BootstrapMethods) {
                BootstrapMethods bootstrapMethods = (BootstrapMethods) attribute;
                BootstrapMethod[] bootstrapMethodArray = bootstrapMethods.getBootstrapMethods();
                if (bootstrapMethodArray != null && bootstrapMethodArray.length > index) {
                    return bootstrapMethodArray[index];
                }
            }
        }

        return null;
    }

    /**
     * 获得BootstrapMethod的方法信息
     *
     * @param bootstrapMethod
     * @param javaClass
     * @return
     */
    public static MethodInfo getBootstrapMethodInfo(BootstrapMethod bootstrapMethod, JavaClass javaClass) {
        for (int argIndex : bootstrapMethod.getBootstrapArguments()) {
            Constant constantArg = javaClass.getConstantPool().getConstant(argIndex);
            if (!(constantArg instanceof ConstantMethodHandle)) {
                continue;
            }

            MethodInfo methodInfo = getMethodFromConstantMethodHandle((ConstantMethodHandle) constantArg, javaClass);
            if (methodInfo != null) {
                return methodInfo;
            }
        }

        return null;
    }

    /**
     * 根据ConstantMethodHandle获得Method对象
     *
     * @param constantMethodHandle
     * @param javaClass
     * @return
     */
    public static MethodInfo getMethodFromConstantMethodHandle(ConstantMethodHandle constantMethodHandle, JavaClass javaClass) {
        ConstantPool constantPool = javaClass.getConstantPool();

        Constant constantCP = constantPool.getConstant(constantMethodHandle.getReferenceIndex());
        if (!(constantCP instanceof ConstantCP)) {
            System.err.println("### 不满足instanceof ConstantCP " + constantCP.getClass().getName());
            return null;
        }

        ConstantCP constantClassAndMethod = (ConstantCP) constantCP;
        String className = constantPool.getConstantString(constantClassAndMethod.getClassIndex(), Const.CONSTANT_Class);
        className = Utility.compactClassName(className, false);

        Constant constantNAT = constantPool.getConstant(constantClassAndMethod.getNameAndTypeIndex());
        if (!(constantNAT instanceof ConstantNameAndType)) {
            System.err.println("### 不满足instanceof ConstantNameAndType " + constantNAT.getClass().getName());
            return null;
        }
        ConstantNameAndType constantNameAndType = (ConstantNameAndType) constantNAT;
        String methodName = constantPool.constantToString(constantNameAndType.getNameIndex(), Const.CONSTANT_Utf8);
        String methodArgs = constantPool.constantToString(constantNameAndType.getSignatureIndex(), Const.CONSTANT_Utf8);

        if (methodName != null && methodArgs != null) {
            MethodInfo methodInfo = new MethodInfo();
            methodInfo.setClassName(className);
            methodInfo.setMethodName(methodName);
            Type[] types = Type.getArgumentTypes(methodArgs);
            methodInfo.setMethodArgumentTypes(types);
            return methodInfo;
        }

        System.err.println("### 获取方法信息失败 " + javaClass.getClassName() + " " + className + " " + methodName + " " + methodArgs);
        return null;
    }

    /**
     * 判断childClassName是否直接或间接继承自superClassName
     *
     * @param childClassName
     * @param superClassName
     * @param extendsClassMethodInfoMap
     * @return
     */
    public static boolean isChildOf(String childClassName, String superClassName, Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap) {
        if (childClassName == null || superClassName == null || extendsClassMethodInfoMap == null) {
            return false;
        }

        String currentClassName = childClassName;
        while (true) {
            ExtendsClassMethodInfo extendsClassMethodInfo = extendsClassMethodInfoMap.get(currentClassName);
            if (extendsClassMethodInfo == null) {
                return false;
            }

            if (superClassName.equals(extendsClassMethodInfo.getSuperClassName())) {
                return true;
            }

            currentClassName = extendsClassMethodInfo.getSuperClassName();
        }
    }

    /**
     * 判断childClassName是否直接或间接实现了interfaceName
     *
     * @param className
     * @param interfaceName
     * @param extendsClassMethodInfoMap
     * @param classInterfaceMethodInfoMap
     * @return
     */
    public static boolean isImplementationOf(String className, String interfaceName, Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap, Map<String,
            ClassInterfaceMethodInfo> classInterfaceMethodInfoMap) {
        if (className == null || interfaceName == null || extendsClassMethodInfoMap == null || classInterfaceMethodInfoMap == null) {
            return false;
        }

        String currentClassName = className;
        while (true) {
            ClassInterfaceMethodInfo classInterfaceMethodInfo = classInterfaceMethodInfoMap.get(currentClassName);
            if (classInterfaceMethodInfo != null && classInterfaceMethodInfo.getInterfaceNameList() != null && classInterfaceMethodInfo.getInterfaceNameList().contains(interfaceName)) {
                return true;
            }

            ExtendsClassMethodInfo extendsClassMethodInfo = extendsClassMethodInfoMap.get(currentClassName);
            if (extendsClassMethodInfo == null) {
                return false;
            }

            currentClassName = extendsClassMethodInfo.getSuperClassName();
        }
    }

    /**
     * 获取指定类className的所有父类及当前类名，排除Object类
     *
     * @param className
     * @param extendsClassMethodInfoMap
     * @return
     */
    public static List<String> getAllClassNameList(String className, Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap) {
        List<String> classNameList = new ArrayList<>();
        if (className == null || extendsClassMethodInfoMap == null) {
            return classNameList;
        }

        String currentClassName = className;
        while (true) {
            classNameList.add(currentClassName);

            ExtendsClassMethodInfo extendsClassMethodInfo = extendsClassMethodInfoMap.get(currentClassName);
            if (extendsClassMethodInfo == null || JavaCGConstants.OBJECT_CLASS_NAME.equals(extendsClassMethodInfo.getSuperClassName())) {
                break;
            }

            currentClassName = extendsClassMethodInfo.getSuperClassName();
        }

        return classNameList;
    }

    public static void debugPrint(String data) {
        if (!debugPrintFlag) {
            return;
        }

        System.out.println(data);
    }

    /**
     * 处理类名中的数组形式
     *
     * @param className
     * @return
     */
    public static String handleClassNameWithArray(String className) {
        if (!className.startsWith("[")) {
            return className;
        }

        // 处理数组格式
        String tmpClassName = Utility.typeSignatureToString(className, false);
        if (!tmpClassName.endsWith(JavaCGConstants.FLAG_ARRAY)) {
            System.err.println("handleClassNameWithArray " + tmpClassName);
            return tmpClassName;
        }

        return tmpClassName.substring(0, tmpClassName.length() - JavaCGConstants.FLAG_ARRAY.length());
    }

    /**
     * 将注解信息写入文件
     *
     * @param type
     * @param classOrMethod
     * @param annotationEntries
     * @param writer
     */
    public static void writeAnnotationInfo(String type, String classOrMethod, AnnotationEntry[] annotationEntries, BufferedWriter writer) {
        if (annotationEntries == null || annotationEntries.length == 0) {
            return;
        }

        try {
            for (AnnotationEntry annotationEntry : annotationEntries) {
                String annotationClassName = Utility.typeSignatureToString(annotationEntry.getAnnotationType(), false);
                String data = type + " " + classOrMethod + " " + annotationClassName;

                if (annotationEntry.getElementValuePairs() == null || annotationEntry.getElementValuePairs().length == 0) {
                    writer.write(data + JavaCGConstants.NEW_LINE);
                }

                for (ElementValuePair elementValuePair : annotationEntry.getElementValuePairs()) {
                    String key = elementValuePair.getNameString();
                    String value = elementValuePair.getValue().toString();
                    value = encodeAnnotationValue(value);

                    writer.write(data + " " + key + " " + value + JavaCGConstants.NEW_LINE);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String encodeAnnotationValue(String value) {
        return value.replace(' ', JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_BACKSPACE)
                .replace('\r', JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN)
                .replace('\n', JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED);
    }

    public static String decodeAnnotationValue(String value) {
        return value.replace(JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_BACKSPACE, ' ')
                .replace(JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_CARRIAGE_RETURN, '\r')
                .replace(JavaCGConstants.ANNOTATION_ATTRIBUTE_VALUE_REPLACE_LINE_FEED, '\n');
    }

    /**
     * 生成格式化后的完整方法名
     *
     * @param fullClassName 完整类名
     * @param methodName    方法名，不包含()
     * @param methodArgs    方法参数，包含起始的()，参数类名之间需要使用半角逗号,分隔，不能包含空格，参数类名也需要为完整类名
     * @return
     */
    public static String formatFullMethod(String fullClassName, String methodName, String methodArgs) {
        return fullClassName + ":" + methodName + methodArgs;
    }

    /**
     * 生成格式化后的方法调用关系
     *
     * @param callId           方法调用序号
     * @param callerFullMethod 调用方法完整方法名
     * @param callType         调用类型
     * @param calleeClassName  被调用方法
     * @param calleeMethodName 被调用方法方法名，说明同上
     * @param calleeMethodArgs 被调用方法方法参数，说明同上
     * @return
     */
    public static String formatMethodCall(int callId, String callerFullMethod, String callType, String calleeClassName, String calleeMethodName, String calleeMethodArgs) {
        return JavaCGConstants.FILE_KEY_METHOD_PREFIX + String.format("%d %s (%s)%s:%s%s", callId, callerFullMethod, callType, calleeClassName, calleeMethodName, calleeMethodArgs);
    }

    private JavaCGUtil() {
        throw new IllegalStateException("illegal");
    }
}