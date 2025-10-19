package com.adrninistrator.javacg2.el.manager;

import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.el.enums.CommonElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.handler.ElHandler;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/8/21
 * @description: 公共的表达式管理类基类
 */
public abstract class CommonElManager extends AbstractElManager {
    protected CommonElManager(BaseConfigureWrapper configureWrapper, ElConfigInterface[] elConfigInterfaces, String outputDirPath) {
        super(configureWrapper, elConfigInterfaces, outputDirPath);
    }

    /**
     * 为方法调用添加数据，类型
     *
     * @param elHandler
     * @param methodCallType
     * @param usedVariableMap
     * @param displayMap
     */
    protected void methodCallAddData4Type(ElHandler elHandler, String methodCallType, Map<String, Object> usedVariableMap, Map<String, Object> displayMap) {
        if (StringUtils.isBlank(methodCallType)) {
            return;
        }
        displayMap.put(CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName(), methodCallType);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName(), methodCallType);
        }
    }

    /**
     * 为方法调用添加数据，调用方法
     *
     * @param elHandler
     * @param callerFullMethod
     * @param usedVariableMap
     * @param displayMap
     */
    protected void methodCallAddData4Caller(ElHandler elHandler, String callerFullMethod, Map<String, Object> usedVariableMap, Map<String, Object> displayMap) {
        if (StringUtils.isBlank(callerFullMethod)) {
            return;
        }

        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName(), callerClassName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName(), callerClassName);
        }

        String callerPackageName = JavaCG2ClassMethodUtil.getPackageName(callerClassName);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName(), callerPackageName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName(), callerPackageName);
        }

        String callerSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(callerClassName);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName(), callerSimpleClassName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName(), callerSimpleClassName);
        }

        String callerMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(callerFullMethod);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName(), callerMethodName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName(), callerMethodName);
        }

        int callerMethodArgNum = JavaCG2ClassMethodUtil.getMethodArgNum(callerFullMethod);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName(), callerMethodArgNum);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName(), callerMethodArgNum);
        }

        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName(), callerFullMethod);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName(), callerFullMethod);
        }
    }

    /**
     * 为方法调用添加数据，被调用方法
     *
     * @param elHandler
     * @param calleeFullMethod
     * @param usedVariableMap
     * @param displayMap
     */
    protected void methodCallAddData4Callee(ElHandler elHandler, String calleeFullMethod, Map<String, Object> usedVariableMap, Map<String, Object> displayMap) {
        if (StringUtils.isBlank(calleeFullMethod)) {
            return;
        }
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName(), calleeClassName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName(), calleeClassName);
        }

        String calleePackageName = JavaCG2ClassMethodUtil.getPackageName(calleeClassName);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName(), calleePackageName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName(), calleePackageName);
        }

        String calleeSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(calleeClassName);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName(), calleeSimpleClassName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName(), calleeSimpleClassName);
        }

        String calleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME.getVariableName(), calleeMethodName);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME.getVariableName(), calleeMethodName);
        }

        int calleeMethodArgNum = JavaCG2ClassMethodUtil.getMethodArgNum(calleeFullMethod);
        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM.getVariableName(), calleeMethodArgNum);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM.getVariableName(), calleeMethodArgNum);
        }

        displayMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName(), calleeFullMethod);
        if (elHandler.checkVariableNameSpecified(CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD)) {
            usedVariableMap.put(CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName(), calleeFullMethod);
        }
    }

    // 解析类时，向Map添加数据
    protected void addData4Class(ElHandler elHandler, Map<String, Object> usedVariableMap, Map<String, Object> displayMap, String className) {
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName(), className);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName(), className);
        }
        String packageName = JavaCG2ClassMethodUtil.getPackageName(className);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName(), packageName);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName(), packageName);
        }

        String simpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName(), simpleClassName);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName(), simpleClassName);
        }
    }

    // 解析方法时，向Map添加数据
    protected void addData4Method(ElHandler elHandler, Map<String, Object> usedVariableMap, Map<String, Object> displayMap, String fullMethod) {
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        // 根据类向Map添加数据
        addData4Class(elHandler, usedVariableMap, displayMap, className);
        // 处理方法名
        String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME.getVariableName(), methodName);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME.getVariableName(), methodName);
        }
        int methodArgNum = JavaCG2ClassMethodUtil.getMethodArgNum(fullMethod);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM.getVariableName(), methodArgNum);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM.getVariableName(), methodArgNum);
        }
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD.getVariableName(), fullMethod);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD.getVariableName(), fullMethod);
        }
    }
}
