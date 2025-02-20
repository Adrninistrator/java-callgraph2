package com.adrninistrator.javacg2.el.manager;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.handler.ElHandler;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 表达式管理类
 */
public class JavaCG2ElManager extends ElManager {

    public JavaCG2ElManager(BaseConfigureWrapper configureWrapper, ElConfigInterface[] elConfigInterfaces) {
        super(configureWrapper, elConfigInterfaces);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的文件
     *
     * @param filePath
     * @return true: 跳过 false: 不跳过
     */
    public boolean checkIgnoreMergeFileInJarWar(String filePath) {
        if (JavaCG2FileUtil.checkJarFile(filePath)) {
            return checkIgnoreMergeJarInJarWar(filePath);
        }
        if (JavaCG2FileUtil.checkWarFile(filePath)) {
            return true;
        }
        if (JavaCG2FileUtil.checkClassFile(filePath)) {
            return checkIgnoreMergeClassInJarWar(filePath);
        }
        return checkIgnoreMergeOtherInJarWar(filePath);
    }

    /**
     * 检查是否需要跳过合并目录中的文件
     *
     * @param fileCanonicalPath
     * @return true: 跳过 false: 不跳过
     */
    public boolean checkIgnoreMergeFileInDir(String fileCanonicalPath) {
        if (JavaCG2FileUtil.checkJarFile(fileCanonicalPath)) {
            return checkIgnoreMergeJarWarInDir(fileCanonicalPath, true);
        }
        if (JavaCG2FileUtil.checkWarFile(fileCanonicalPath)) {
            return checkIgnoreMergeJarWarInDir(fileCanonicalPath, false);
        }
        if (JavaCG2FileUtil.checkClassFile(fileCanonicalPath)) {
            return checkIgnoreMergeClassInDir(fileCanonicalPath);
        }
        return checkIgnoreMergeOtherInDir(fileCanonicalPath);
    }

    // 合并jar文件时，向Map添加目录中的文件的数据
    private void mergeFileAddData4FileInDir(ElHandler elHandler, Map<String, Object> map, String filePath) {
        // 将文件路径中的反斜杠替换为斜杠
        String newFilePath = JavaCG2FileUtil.replaceFilePath2Slash(filePath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR.getVariableName(), newFilePath);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME)) {
            String fileName = JavaCG2FileUtil.getFileNameSupportSlash(newFilePath);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName(), fileName);
        }
    }

    // 合并jar文件时，向Map添加jar/war文件中的文件的数据
    private void mergeFileAddData4FileInJarWar(ElHandler elHandler, Map<String, Object> map, String filePath) {
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR.getVariableName(), filePath);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME)) {
            String fileName = JavaCG2FileUtil.getFileNameSupportSlash(filePath);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName(), fileName);
        }
    }

    /**
     * 检查是否需要跳过合并目录中的jar/war文件
     *
     * @param fileCanonicalPath
     * @param jarOrWarFile
     * @return
     */
    private boolean checkIgnoreMergeJarWarInDir(String fileCanonicalPath, boolean jarOrWarFile) {
        ElHandler elHandler;
        if (jarOrWarFile) {
            elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_DIR);
        } else {
            elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_WAR_IN_DIR);
        }
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        mergeFileAddData4FileInDir(elHandler, map, fileCanonicalPath);
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过合并目录中的class文件
     *
     * @param fileCanonicalPath
     * @return
     */
    private boolean checkIgnoreMergeClassInDir(String fileCanonicalPath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        mergeFileAddData4FileInDir(elHandler, map, fileCanonicalPath);
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过合并目录中非jar、war、class文件
     *
     * @param fileCanonicalPath
     * @return
     */
    private boolean checkIgnoreMergeOtherInDir(String fileCanonicalPath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        mergeFileAddData4FileInDir(elHandler, map, fileCanonicalPath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT_LOWER)) {
            String fileExtLower = JavaCG2FileUtil.getFileExtLower(fileCanonicalPath);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT_LOWER.getVariableName(), fileExtLower);
        }
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的jar文件
     *
     * @param filePath
     * @return
     */
    private boolean checkIgnoreMergeJarInJarWar(String filePath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        mergeFileAddData4FileInJarWar(elHandler, map, filePath);
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的class文件
     *
     * @param filePath
     * @return
     */
    private boolean checkIgnoreMergeClassInJarWar(String filePath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        mergeFileAddData4FileInJarWar(elHandler, map, filePath);
        // 处理class文件路径
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR)) {
            String classFilePath = filePath;
            if (filePath.startsWith(JavaCG2Constants.BOOT_INF_CLASSES)) {
                classFilePath = StringUtils.substringAfter(filePath, JavaCG2Constants.BOOT_INF_CLASSES);
            } else if (filePath.startsWith(JavaCG2Constants.WEB_INF_CLASSES)) {
                classFilePath = StringUtils.substringAfter(filePath, JavaCG2Constants.WEB_INF_CLASSES);
            }
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR.getVariableName(), classFilePath);
        }
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的非jar、war、class文件
     *
     * @param filePath
     * @return
     */
    private boolean checkIgnoreMergeOtherInJarWar(String filePath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        mergeFileAddData4FileInJarWar(elHandler, map, filePath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT_LOWER)) {
            String fileExtLower = JavaCG2FileUtil.getFileExtLower(filePath);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT_LOWER.getVariableName(), fileExtLower);
        }
        return elHandler.runExpression(map);
    }

    // 解析类时，向Map添加目录中的文件的数据
    private void parseAddData4Class(ElHandler elHandler, Map<String, Object> map, String className) {
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName(), className);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME)) {
            String packageName = JavaCG2ClassMethodUtil.getPackageName(className);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName(), packageName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME)) {
            String simpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName(), simpleClassName);
        }
    }

    /**
     * 检查是否需要跳过解析类
     *
     * @param className
     * @return
     */
    public boolean checkIgnoreParseClass(String className) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        parseAddData4Class(elHandler, map, className);
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过解析方法
     *
     * @param fullMethod
     * @return
     */
    public boolean checkIgnoreParseMethod(String fullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        parseAddData4Class(elHandler, map, className);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME)) {
            String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME.getVariableName(), methodName);
        }
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过记录方法调用，只通过调用方法判断
     *
     * @param callerFullMethod 调用方完整方法
     * @return
     */
    public boolean checkIgnoreMethodCallByEr(String callerFullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        methodCallAddData4Caller(elHandler, callerFullMethod, map);
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过记录方法调用，只通过被调用方法判断
     *
     * @param calleeFullMethod 被调用方完整方法
     * @return
     */
    public boolean checkIgnoreMethodCallByEe(String calleeFullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_EE);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        methodCallAddData4Callee(elHandler, calleeFullMethod, map);
        return elHandler.runExpression(map);
    }

    /**
     * 检查是否需要跳过记录方法调用，通过调用方法与被调用方法一起判断
     *
     * @param callerFullMethod 调用方完整方法
     * @param calleeFullMethod 被调用方完整方法
     * @return
     */
    public boolean checkIgnoreMethodCallByErEe(String callerFullMethod, String calleeFullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_ER_EE);
        Map<String, Object> map = elHandler.genMap4ElExecute();
        if (map == null) {
            return false;
        }
        methodCallAddData4Caller(elHandler, callerFullMethod, map);
        methodCallAddData4Callee(elHandler, calleeFullMethod, map);
        return elHandler.runExpression(map);
    }

    private void methodCallAddData4Caller(ElHandler elHandler, String callerFullMethod, Map<String, Object> map) {
        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName(), callerClassName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME)) {
            String callerPackageName = JavaCG2ClassMethodUtil.getPackageName(callerClassName);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName(), callerPackageName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME)) {
            String callerSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(callerClassName);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName(), callerSimpleClassName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME)) {
            String callerMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(callerFullMethod);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName(), callerMethodName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM)) {
            int callerMethodArgNum = JavaCG2ClassMethodUtil.getMethodArgNum(callerFullMethod);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName(), callerMethodArgNum);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName(), callerFullMethod);
        }
    }

    private void methodCallAddData4Callee(ElHandler elHandler, String calleeFullMethod, Map<String, Object> map) {
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName(), calleeClassName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME)) {
            String calleePackageName = JavaCG2ClassMethodUtil.getPackageName(calleeClassName);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName(), calleePackageName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME)) {
            String calleeSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(calleeClassName);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName(), calleeSimpleClassName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME)) {
            String calleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME.getVariableName(), calleeMethodName);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM)) {
            int calleeMethodArgNum = JavaCG2ClassMethodUtil.getMethodArgNum(calleeFullMethod);
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM.getVariableName(), calleeMethodArgNum);
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD)) {
            map.put(JavaCG2ElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName(), calleeFullMethod);
        }
    }
}
