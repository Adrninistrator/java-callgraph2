package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.type.JavaCG2GenericsType;
import copy.javassist.bytecode.SignatureAttribute;
import org.apache.bcel.classfile.Utility;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description: 类、方法、字段定义签名处理工具类
 */
public class JavaCG2GenericsTypeUtil {

    /**
     * 解析类型定义中的泛型类型
     *
     * @param type             需要解析的类型定义
     * @param record           是否需要记录泛型类型
     * @param genericsTypeList 保存解析后的泛型类型列表
     */
    public static void parseTypeDefineGenericsType(SignatureAttribute.Type type, boolean record, List<JavaCG2GenericsType> genericsTypeList) {
        parseTypeDefineGenericsType(type, record, 0, null, genericsTypeList);
    }

    /**
     * 解析类型定义中的泛型类型
     *
     * @param type             需要解析的类型定义
     * @param record           是否需要记录泛型类型
     * @param arrayDimensions  属于数组类型时的数组维度，0代表不是数组
     * @param wildcard         通配符
     * @param genericsTypeList 保存解析后的泛型类型列表
     */
    public static void parseTypeDefineGenericsType(SignatureAttribute.Type type, boolean record, int arrayDimensions, String wildcard, List<JavaCG2GenericsType> genericsTypeList) {
        if (type instanceof SignatureAttribute.TypeVariable) {
            // 类型变量
            SignatureAttribute.TypeVariable typeVariable = (SignatureAttribute.TypeVariable) type;
            JavaCG2GenericsType javaCG2GenericsType = new JavaCG2GenericsType();
            javaCG2GenericsType.setTypeVariablesName(typeVariable.getName());
            javaCG2GenericsType.setArrayDimensions(arrayDimensions);
            genericsTypeList.add(javaCG2GenericsType);
        } else if (type instanceof SignatureAttribute.BaseType) {
            // 基本类型
            SignatureAttribute.BaseType baseType = (SignatureAttribute.BaseType) type;
            JavaCG2GenericsType javaCG2GenericsType = new JavaCG2GenericsType();
            String baseTypeStr = JavaCG2GenericsTypeUtil.getBaseTypeStr(baseType);
            javaCG2GenericsType.setType(baseTypeStr);
            javaCG2GenericsType.setArrayDimensions(arrayDimensions);
            genericsTypeList.add(javaCG2GenericsType);
        } else if (type instanceof SignatureAttribute.ClassType) {
            // 类类型
            SignatureAttribute.ClassType classType = (SignatureAttribute.ClassType) type;
            if (record) {
                /*
                    需要记录泛型类型
                 */
                JavaCG2GenericsType javaCG2GenericsType = new JavaCG2GenericsType();
                javaCG2GenericsType.setArrayDimensions(arrayDimensions);
                javaCG2GenericsType.setWildcard(wildcard);
                String typeStr = JavaCG2GenericsTypeUtil.getClassName(classType);
                if (wildcard != null) {
                    javaCG2GenericsType.setReferenceType(typeStr);
                } else {
                    javaCG2GenericsType.setType(typeStr);
                }
                genericsTypeList.add(javaCG2GenericsType);
            }

            // 获取参数类型
            SignatureAttribute.TypeArgument[] typeArguments = classType.getTypeArguments();
            if (ArrayUtils.isEmpty(typeArguments)) {
                return;
            }

            // 遍历参数类型
            for (SignatureAttribute.TypeArgument typeArgument : typeArguments) {
                SignatureAttribute.ObjectType typeInTypeArgument = typeArgument.getType();
                String tmpWildcard = getWildcard(typeArgument.getKind());
                if (typeInTypeArgument != null) {
                    // TypeArgument中的类型非空，递归处理
                    parseTypeDefineGenericsType(typeInTypeArgument, true, arrayDimensions, tmpWildcard, genericsTypeList);
                    continue;
                }
                // TypeArgument中的类型为空
                JavaCG2GenericsType javaCG2GenericsType = new JavaCG2GenericsType();
                javaCG2GenericsType.setWildcard(tmpWildcard);
                genericsTypeList.add(javaCG2GenericsType);
            }
        } else if (type instanceof SignatureAttribute.ArrayType) {
            // 数组类型
            SignatureAttribute.ArrayType arrayType = (SignatureAttribute.ArrayType) type;
            // 递归处理
            parseTypeDefineGenericsType(arrayType.getComponentType(), true, arrayType.getDimension(), wildcard, genericsTypeList);
        }
    }

    /**
     * 获得泛型类型中的通配符
     *
     * @param kind
     * @return
     */
    public static String getWildcard(char kind) {
        switch (kind) {
            case '*':
                return "?";
            case '+':
                return "extends";
            case '-':
                return "super";
            default:
                return null;
        }
    }

    /**
     * 获取指定类的类名，支持内部类
     *
     * @param classType
     * @return
     */
    public static String getClassName(SignatureAttribute.ClassType classType) {
        if (!(classType instanceof SignatureAttribute.NestedClassType)) {
            // 非内部类的情况
            return classType.getName();
        }
        // 内部类的情况
        List<String> classNameList = new ArrayList<>();
        SignatureAttribute.ClassType currentClassType = classType;
        while (true) {
            classNameList.add(0, currentClassType.getName());
            if (!(currentClassType instanceof SignatureAttribute.NestedClassType)) {
                break;
            }
            currentClassType = currentClassType.getDeclaringClass();
        }
        return StringUtils.join(classNameList, JavaCG2Constants.FLAG_DOLOR);
    }

    /**
     * 根据SignatureAttribute.BaseType获得对应的类型字符串
     *
     * @param baseType
     * @return
     */
    public static String getBaseTypeStr(SignatureAttribute.BaseType baseType) {
        return Utility.typeSignatureToString(String.valueOf(baseType.getDescriptor()), false);
    }

    /**
     * 生成文件中多列形式的泛型信息字符串
     *
     * @param genericsSeq
     * @param javaCG2GenericsType
     * @return
     */
    public static String genGenericsTypeStr(int genericsSeq, JavaCG2GenericsType javaCG2GenericsType) {
        return JavaCG2FileUtil.appendFileColumn(
                String.valueOf(genericsSeq),
                javaCG2GenericsType.getType(),
                String.valueOf(javaCG2GenericsType.getArrayDimensions()),
                javaCG2GenericsType.getTypeVariablesName(),
                javaCG2GenericsType.getWildcard(),
                javaCG2GenericsType.getReferenceType(),
                JavaCG2ClassMethodUtil.getClassCategory(javaCG2GenericsType.getType()));
    }

    /**
     * 生成文件中多列形式的泛型信息字符串，用于类继承或实现时的泛型信息
     *
     * @param genericsSeq
     * @param javaCG2GenericsType
     * @return
     */
    public static String genGenericsTypeStr4ClassExtImpl(int genericsSeq, JavaCG2GenericsType javaCG2GenericsType) {
        return JavaCG2FileUtil.appendFileColumn(
                String.valueOf(genericsSeq),
                javaCG2GenericsType.getType(),
                String.valueOf(javaCG2GenericsType.getArrayDimensions()),
                javaCG2GenericsType.getTypeVariablesName(),
                JavaCG2ClassMethodUtil.getClassCategory(javaCG2GenericsType.getType()));
    }

    private JavaCG2GenericsTypeUtil() {
        throw new IllegalStateException("illegal");
    }
}
