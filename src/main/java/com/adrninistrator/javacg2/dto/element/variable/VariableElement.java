package com.adrninistrator.javacg2.dto.element.variable;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.dto.variabledatasource.AbstractVariableDataSource;
import com.adrninistrator.javacg2.dto.variabledatasource.VariableDataSourceMethodCallReturn;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/5/13
 * @description: 变量基类
 */
public class VariableElement extends BaseElement {

    // 变量的数据来源
    private AbstractVariableDataSource variableDataSource;

    // 等值转换前的变量的数据来源
    private AbstractVariableDataSource variableDataSourceEQC;

    // catch代码块的开始指令偏移量，当前值非null时代表当前元素是catch的异常对象
    private Integer catchExceptionStartPosition;

    private VariableElement() {
    }

    public VariableElement(String type) {
        super(type, 0);
    }

    public VariableElement(String type, int addArrayDimensions) {
        super(type, addArrayDimensions);
    }

    @Override
    public BaseElement copyElement() {
        VariableElement copyVariableElement = new VariableElement();
        copyVariableElement.setType(getType());
        copyVariableElement.setArrayDimensions(getArrayDimensions());
        copyVariableElement.setArrayValueMap(getArrayValueMap());
        copyVariableElement.setValue(getValue());
        return copyVariableElement;
    }

    /**
     * 拷贝指定元素的变量数据来源
     *
     * @param srcBaseElement
     */
    public void copyVariableDataSource(BaseElement srcBaseElement) {
        if (srcBaseElement instanceof VariableElement) {
            VariableElement srcValueVariableElement = (VariableElement) srcBaseElement;
            AbstractVariableDataSource srcVariableDataSource = srcValueVariableElement.variableDataSource;
            if (srcVariableDataSource != null) {
                this.variableDataSource = srcVariableDataSource;
            }
            AbstractVariableDataSource srcVariableDataSourceEQC = srcValueVariableElement.variableDataSourceEQC;
            if (srcVariableDataSourceEQC != null) {
                this.variableDataSourceEQC = srcVariableDataSourceEQC;
            }
        }
    }

    /**
     * 记录变量的数据来源
     *
     * @param variableDataSource      变量的数据来源
     * @param frEqConversionMethodMap 等值转换的方法Map
     */
    public void recordVariableDataSource(AbstractVariableDataSource variableDataSource, Map<String, Map<String, Integer>> frEqConversionMethodMap) {
        this.variableDataSource = variableDataSource;
        if (variableDataSource instanceof VariableDataSourceMethodCallReturn) {
            // 当变量的数据来源为方法调用返回值时，对等转换的方法进行处理
            handleVariableDataSourceEQC(variableDataSource, frEqConversionMethodMap);
        }
    }

    // 对等转换的方法进行处理
    private void handleVariableDataSourceEQC(AbstractVariableDataSource variableDataSource, Map<String, Map<String, Integer>> frEqConversionMethodMap) {
        VariableDataSourceMethodCallReturn variableDataSourceMethodCallReturn = (VariableDataSourceMethodCallReturn) variableDataSource;
        Map<String, Integer> methodMap = frEqConversionMethodMap.get(variableDataSourceMethodCallReturn.getCalleeClassName());
        if (methodMap == null) {
            return;
        }
        Integer objArgSeq = methodMap.get(variableDataSourceMethodCallReturn.getCalleeMethodName());
        if (objArgSeq == null) {
            return;
        }
        // 当前数据来源的方法调用的被调用方法属于等值转换
        if (objArgSeq == JavaCG2Constants.METHOD_CALL_OBJECT_SEQ) {
            // 使用被调用对象作为等值转换前的数据来源
            BaseElement objElement = variableDataSourceMethodCallReturn.getObjectElement();
            // 记录变量等值转换前的数据来源
            recordVariableDataSourceEQC(objElement);
            return;
        }
        // 使用被调用参数作为等值转换前的数据来源
        List<BaseElement> argElementList = variableDataSourceMethodCallReturn.getArgElementList();
        if (JavaCG2Util.isCollectionEmpty(argElementList) || argElementList.size() < objArgSeq) {
            // 若未找到对应参数则不处理
            return;
        }

        BaseElement argElement = argElementList.get(objArgSeq - 1);
        // 记录变量等值转换前的数据来源
        recordVariableDataSourceEQC(argElement);
    }

    // 记录变量等值转换前的数据来源
    private void recordVariableDataSourceEQC(BaseElement baseElement) {
        // 假如变量等值转换前的数据来源非空，则使用
        AbstractVariableDataSource argVariableDataSourceEQC = getVariableDataSourceEQCFromElement(baseElement);
        if (argVariableDataSourceEQC != null) {
            this.variableDataSourceEQC = argVariableDataSourceEQC;
            return;
        }
        // 假如变量等值转换前的数据来源为空，则使用变量的数据来源
        AbstractVariableDataSource argVariableDataSource = getVariableDataSourceFromElement(baseElement);
        if (argVariableDataSource != null) {
            this.variableDataSourceEQC = argVariableDataSource;
        }
    }

    /**
     * 获取元素的变量数据来源
     *
     * @param baseElement
     * @return
     */
    private AbstractVariableDataSource getVariableDataSourceFromElement(BaseElement baseElement) {
        if (!(baseElement instanceof VariableElement)) {
            return null;
        }
        return ((VariableElement) baseElement).variableDataSource;
    }

    /**
     * 获取元素的等值转换前的变量数据来源
     *
     * @param baseElement
     * @return
     */
    private AbstractVariableDataSource getVariableDataSourceEQCFromElement(BaseElement baseElement) {
        if (!(baseElement instanceof VariableElement)) {
            return null;
        }
        return ((VariableElement) baseElement).variableDataSourceEQC;
    }

    public AbstractVariableDataSource getVariableDataSource() {
        return variableDataSource;
    }

    public void setVariableDataSource(AbstractVariableDataSource variableDataSource) {
        this.variableDataSource = variableDataSource;
    }

    public AbstractVariableDataSource getVariableDataSourceEQC() {
        return variableDataSourceEQC;
    }

    public void setVariableDataSourceEQC(AbstractVariableDataSource variableDataSourceEQC) {
        this.variableDataSourceEQC = variableDataSourceEQC;
    }

    public Integer getCatchExceptionStartPosition() {
        return catchExceptionStartPosition;
    }

    public void setCatchExceptionStartPosition(Integer catchExceptionStartPosition) {
        this.catchExceptionStartPosition = catchExceptionStartPosition;
    }
}
