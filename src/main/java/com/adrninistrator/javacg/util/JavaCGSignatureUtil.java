package com.adrninistrator.javacg.util;

import com.adrninistrator.javacg.common.JavaCGConstants;
import copy.javassist.bytecode.SignatureAttribute;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description: 类、方法、字段定义签名处理工具类
 */
public class JavaCGSignatureUtil {

    /**
     * 解析类型定义中的泛型类型
     *
     * @param type             需要解析的类型定义
     * @param outer            是否为外层的数据
     * @param genericsTypeList 保存解析后的泛型类型列表
     */
    public static void parseTypeDefineGenericsType(SignatureAttribute.Type type, boolean outer, List<String> genericsTypeList) {
        if (!(type instanceof SignatureAttribute.ClassType)) {
            return;
        }

        SignatureAttribute.ClassType classType = (SignatureAttribute.ClassType) type;
        // 获取参数类型
        SignatureAttribute.TypeArgument[] typeArguments = classType.getTypeArguments();
        if (ArrayUtils.isEmpty(typeArguments)) {
            if (outer) {
                // 外层的数据，没有泛型类型，不需要记录，返回
                return;
            }

            // 当前参数类型下不再有类型
            genericsTypeList.add(JavaCGSignatureUtil.getClassName(classType));
            return;
        }

        if (!outer) {
            // 内层的数据，当前参数类型下还有类型，记录当前参数的类型，如List、Map
            genericsTypeList.add(JavaCGSignatureUtil.getClassName(classType));
        }
        // 遍历参数类型
        for (SignatureAttribute.TypeArgument typeArgument : typeArguments) {
            // 递归处理，泛型类型需要记录
            parseTypeDefineGenericsType(typeArgument.getType(), false, genericsTypeList);
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
        return StringUtils.join(classNameList, JavaCGConstants.FLAG_DOLOR);
    }

    private JavaCGSignatureUtil() {
        throw new IllegalStateException("illegal");
    }
}
