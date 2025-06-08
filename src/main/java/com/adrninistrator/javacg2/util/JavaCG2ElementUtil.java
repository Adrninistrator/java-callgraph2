package com.adrninistrator.javacg2.util;

import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeObjTypeEnum;
import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.dto.element.variable.FieldElement;
import com.adrninistrator.javacg2.dto.element.variable.LocalVariableElement;
import com.adrninistrator.javacg2.dto.element.variable.StaticFieldElement;
import com.adrninistrator.javacg2.dto.element.variable.StaticFieldMethodCallReturnElement;
import com.adrninistrator.javacg2.dto.element.variable.VariableElement;
import com.adrninistrator.javacg2.dto.variabledatasource.AbstractVariableDataSource;
import com.adrninistrator.javacg2.dto.variabledatasource.VariableDataSourceArithmeticOperation;
import com.adrninistrator.javacg2.dto.variabledatasource.VariableDataSourceMethodArg;
import com.adrninistrator.javacg2.dto.variabledatasource.VariableDataSourceMethodCallReturn;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2022/11/2
 * @description:
 */
public class JavaCG2ElementUtil {
    /**
     * 获取元素类型
     *
     * @param baseElement
     * @return
     */
    public static String getElementType(BaseElement baseElement) {
        if (baseElement == null) {
            return null;
        }
        return baseElement.getType();
    }

    /**
     * 比较已存在的对象与另一个准备添加的对象值是否相同
     *
     * @param existed
     * @param added
     * @return false: 不相同 true: 相同
     */
    public static boolean compare(BaseElement existed, BaseElement added) {
        String addedType = added.getType();
        if (JavaCG2ByteCodeUtil.isNullType(addedType)) {
            // 需要添加的元素类型为null，返回相同
            // 假如需要判断对象是否可能为null，这里需要修改
            return true;
        }

        // 判断元素的类的类型
        String existedClassName = existed.getClass().getName();
        String addedClassName = added.getClass().getName();
        if (!existedClassName.equals(addedClassName)) {
            return false;
        }

        // 判断元素类型
        String existedType = existed.getType();
        if (JavaCG2ByteCodeUtil.isNullType(existedType) || !JavaCG2ByteCodeUtil.compareType(addedType, existedType)) {
            /*
                1. 假如需要添加的类型不是null则继续判断是否与已存在的类型不同（若需要添加的类型为null则不通过类型判断是否不同，继续判断后续条件）
                2. 假如已存在的类型是null，或者已存在的类型与需要添加的类型不同时，返回不相同
             */
            return false;
        }

        // 判断值
        Object addedValue = added.getValue();
        if (addedValue != null && !addedValue.equals(existed.getValue())) {
            /*
                假如需要添加的值非空，且与已存在的值不同时，返回不相同
                （假如需要添加的值为空，则不通过值判断是否不同，继续判断后续条件）
             */
            return false;
        }

        if (existed instanceof VariableElement && added instanceof VariableElement) {
            // 对于变量，需要比较数据来源
            AbstractVariableDataSource existedVariableDataSource = ((VariableElement) existed).getVariableDataSource();
            AbstractVariableDataSource addedVariableDataSource = ((VariableElement) added).getVariableDataSource();
            if (!compareVariableDataSource(existedVariableDataSource, addedVariableDataSource)) {
                return false;
            }
        }

        if (existed instanceof LocalVariableElement && added instanceof LocalVariableElement) {
            // 对于本地变量，需要比较名称
            LocalVariableElement existedLocalVariableElement = (LocalVariableElement) existed;
            LocalVariableElement addedLocalVariableElement = (LocalVariableElement) added;
            if (addedLocalVariableElement.getName() != null && !StringUtils.equals(existedLocalVariableElement.getName(), addedLocalVariableElement.getName())) {
                return false;
            }
        }

        if (existed instanceof FieldElement && added instanceof FieldElement) {
            // 对于字段（非静态或静态），还需要比较字段所在类名
            FieldElement existedFieldElement = (FieldElement) existed;
            FieldElement addedFieldElement = (FieldElement) added;
            if (!StringUtils.equals(existedFieldElement.getClassName(), addedFieldElement.getClassName()) ||
                    !StringUtils.equals(existedFieldElement.getRawType(), addedFieldElement.getRawType())) {
                return false;
            }
        }
        if (existed instanceof StaticFieldMethodCallReturnElement && added instanceof StaticFieldMethodCallReturnElement) {
            // 对于静态字段的方法调用，还需要比较字段所在类名
            StaticFieldMethodCallReturnElement existedStaticFieldMethodCallReturnElement = (StaticFieldMethodCallReturnElement) existed;
            StaticFieldMethodCallReturnElement addedStaticFieldMethodCallReturnElement = (StaticFieldMethodCallReturnElement) added;
            if (!StringUtils.equals(existedStaticFieldMethodCallReturnElement.getClassName(), addedStaticFieldMethodCallReturnElement.getClassName()) ||
                    !StringUtils.equals(existedStaticFieldMethodCallReturnElement.getFieldType(), addedStaticFieldMethodCallReturnElement.getFieldType()) ||
                    !StringUtils.equals(existedStaticFieldMethodCallReturnElement.getMethodName(), addedStaticFieldMethodCallReturnElement.getMethodName())) {
                return false;
            }
            String existedArgTypeStr = JavaCG2ClassMethodUtil.genArgTypeStr(existedStaticFieldMethodCallReturnElement.getMethodArgTypes());
            String addedArgTypeStr = JavaCG2ClassMethodUtil.genArgTypeStr(addedStaticFieldMethodCallReturnElement.getMethodArgTypes());
            if (!StringUtils.equals(existedArgTypeStr, addedArgTypeStr)) {
                return false;
            }
        }
        // 对于需要添加的对象和已存在的对象都属于数组，且确实不相同的情况，不进行处理，意义不大
        return true;
    }

    /**
     * 获取VariableElement的类名或this
     *
     * @param variableElement
     * @return
     */
    public static String getVariableClassNameOrThis(VariableElement variableElement) {
        if (variableElement instanceof LocalVariableElement) {
            if (((LocalVariableElement) variableElement).isThis()) {
                return JavaCG2Constants.THIS;
            }
        }
        return variableElement.getType();
    }

    /**
     * 比较已存在的数据来源与另一个数据来源是否相同
     *
     * @param existed
     * @param added
     * @return false: 不相同 true: 相同
     */
    public static boolean compareVariableDataSource(AbstractVariableDataSource existed, AbstractVariableDataSource added) {
        if (added == null) {
            // 需要添加的数据来源为null时，返回相同
            return true;
        }
        if (existed == null) {
            // 需要添加的数据来源非null，且已存在的为null时，返回不相同
            return false;
        }
        // 需要添加的数据来源和已存在均非null
        // 比较类型
        String existedType = existed.getClass().getName();
        String addedType = added.getClass().getName();
        if (!existedType.equals(addedType)) {
            return false;
        }

        if (existed instanceof VariableDataSourceArithmeticOperation) {
            VariableDataSourceArithmeticOperation existedVariableDataSourceArithmeticOperation = (VariableDataSourceArithmeticOperation) existed;
            VariableDataSourceArithmeticOperation addedVariableDataSourceArithmeticOperation = (VariableDataSourceArithmeticOperation) added;
            return existedVariableDataSourceArithmeticOperation.compare(addedVariableDataSourceArithmeticOperation);
        }

        if (existed instanceof VariableDataSourceMethodArg) {
            VariableDataSourceMethodArg existedVariableDataSourceMethodArg = (VariableDataSourceMethodArg) existed;
            VariableDataSourceMethodArg addedVariableDataSourceMethodArg = (VariableDataSourceMethodArg) added;
            return existedVariableDataSourceMethodArg.compare(addedVariableDataSourceMethodArg);
        }

        if (existed instanceof VariableDataSourceMethodCallReturn) {
            VariableDataSourceMethodCallReturn existedVariableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) existed;
            VariableDataSourceMethodCallReturn addedVariableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) added;
            return existedVariableDataSourceMethodCallReturn.compare(addedVariableDataSourceMethodCallReturn);
        }

        // 其他情况下，返回相同
        return true;
    }

    /**
     * 获取被调用对象类型
     *
     * @param objectElement
     * @return
     */
    public static JavaCG2CalleeObjTypeEnum getCalleeObjTypeEnum(BaseElement objectElement) {
        if (objectElement instanceof StaticFieldElement) {
            return JavaCG2CalleeObjTypeEnum.COTE_STATIC_FIELD;
        }

        if (objectElement instanceof FieldElement) {
            return JavaCG2CalleeObjTypeEnum.COTE_FIELD;
        }

        if (objectElement instanceof LocalVariableElement) {
            LocalVariableElement objLocalVariableElement = (LocalVariableElement) objectElement;
            if (objLocalVariableElement.isThis()) {
                return JavaCG2CalleeObjTypeEnum.COTE_THIS;
            }
        }

        if (objectElement instanceof VariableElement) {
            return JavaCG2CalleeObjTypeEnum.COTE_VARIABLE;
        }
        return null;
    }

    /**
     * 判断指定的元素的数据来源是否为方法参数
     *
     * @param baseElement
     * @return
     */
    public static boolean checkElementDataSourceGetArg(BaseElement baseElement) {
        if (!(baseElement instanceof VariableElement)) {
            return false;
        }
        VariableElement variableElement = (VariableElement) baseElement;
        return variableElement.getVariableDataSource() instanceof VariableDataSourceMethodArg;
    }

    /**
     * 判断方法调用返回变量数据来源是否可能是dto的get方法
     *
     * @param variableDataSourceMethodCallReturn
     * @return true: 可能是dto的get方法 false: 不可能是dto的get方法
     */
    public static boolean checkDataSourceMethodReturnGetMethod(VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn) {
        return !JavaCG2CommonNameConstants.SIMPLE_CLASS_NAME_INVOKE_STATIC.equals(variableDataSourceMethodCallReturn.getInvokeInstructionType()) &&
                JavaCG2ClassMethodUtil.matchesGetMethod(variableDataSourceMethodCallReturn.getCalleeMethodName()) &&
                JavaCG2Constants.EMPTY_METHOD_ARGS.equals(variableDataSourceMethodCallReturn.getCalleeArgTypeStr());
    }

    private JavaCG2ElementUtil() {
        throw new IllegalStateException("illegal");
    }
}
