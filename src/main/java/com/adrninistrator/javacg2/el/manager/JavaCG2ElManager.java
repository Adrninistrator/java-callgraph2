package com.adrninistrator.javacg2.el.manager;

import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElAllowedVariableEnum;
import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.handler.ElHandler;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 表达式管理类
 */
public class JavaCG2ElManager extends CommonElManager {

    public JavaCG2ElManager(BaseConfigureWrapper configureWrapper, ElConfigInterface[] elConfigInterfaces, String outputDirPath) {
        super(configureWrapper, elConfigInterfaces, outputDirPath);
    }

    @Override
    protected boolean chooseDebugMode(BaseConfigureWrapper configureWrapper) {
        return configureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_EL_DEBUG_MODE);
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

    // 合并jar文件时，向Map添加数据
    private void mergeFileAddData4FileInDir(ElHandler elHandler, Map<String, Object> usedVariableMap, Map<String, Object> displayMap, String filePath) {
        // 将文件路径中的反斜杠替换为斜杠
        String newFilePath = JavaCG2FileUtil.replaceFilePath2Slash(filePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR.getVariableName(), newFilePath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR.getVariableName(), newFilePath);
        }

        String fileDirPath = JavaCG2FileUtil.getFileDirPathSupportSlash(newFilePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName(), fileDirPath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName(), fileDirPath);
        }

        String fileName = JavaCG2FileUtil.getFileNameSupportSlash(newFilePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName(), fileName);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName(), fileName);
        }
    }

    // 合并jar文件时，向Map添加jar/war文件中的文件的数据
    private void mergeFileAddData4FileInJarWar(ElHandler elHandler, Map<String, Object> usedVariableMap, Map<String, Object> displayMap, String filePath) {
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR.getVariableName(), filePath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR.getVariableName(), filePath);
        }

        String fileDirPath = JavaCG2FileUtil.getFileDirPathSupportSlash(filePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName(), fileDirPath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName(), fileDirPath);
        }

        String fileName = JavaCG2FileUtil.getFileNameSupportSlash(filePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName(), fileName);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName(), fileName);
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
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        mergeFileAddData4FileInDir(elHandler, usedVariableMap, displayMap, fileCanonicalPath);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过合并目录中的class文件
     *
     * @param fileCanonicalPath
     * @return
     */
    private boolean checkIgnoreMergeClassInDir(String fileCanonicalPath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_DIR);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        mergeFileAddData4FileInDir(elHandler, usedVariableMap, displayMap, fileCanonicalPath);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过合并目录中非jar、war、class文件
     *
     * @param fileCanonicalPath
     * @return
     */
    private boolean checkIgnoreMergeOtherInDir(String fileCanonicalPath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        mergeFileAddData4FileInDir(elHandler, usedVariableMap, displayMap, fileCanonicalPath);

        String fileExt = JavaCG2FileUtil.getFileExt(fileCanonicalPath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName(), fileExt);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName(), fileExt);
        }
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的jar文件
     *
     * @param filePath
     * @return
     */
    private boolean checkIgnoreMergeJarInJarWar(String filePath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        mergeFileAddData4FileInJarWar(elHandler, usedVariableMap, displayMap, filePath);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的class文件
     *
     * @param filePath
     * @return
     */
    private boolean checkIgnoreMergeClassInJarWar(String filePath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        mergeFileAddData4FileInJarWar(elHandler, usedVariableMap, displayMap, filePath);
        // 处理class文件路径
        String classFileRelativelyPath = JavaCG2FileUtil.getClassFileRelativelyPathInJar(filePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR.getVariableName(), classFileRelativelyPath);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_FILE_PATH_IN_JAR_WAR.getVariableName(), classFileRelativelyPath);
        }
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过合并jar/war文件中的非jar、war、class文件
     *
     * @param filePath
     * @return
     */
    private boolean checkIgnoreMergeOtherInJarWar(String filePath) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        mergeFileAddData4FileInJarWar(elHandler, usedVariableMap, displayMap, filePath);
        String fileExt = JavaCG2FileUtil.getFileExt(filePath);
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName(), fileExt);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName(), fileExt);
        }
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过解析类
     *
     * @param className
     * @return
     */
    public boolean checkIgnoreParseClass(String className) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        addData4Class(elHandler, usedVariableMap, displayMap, className);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过解析方法
     *
     * @param fullMethod
     * @return
     */
    public boolean checkIgnoreParseMethod(String fullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        addData4Method(elHandler, usedVariableMap, displayMap, fullMethod);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过记录方法调用
     *
     * @param methodCallType   方法调用类型
     * @param callerFullMethod 调用方完整方法
     * @param calleeFullMethod 被调用方完整方法
     * @return
     */
    public boolean checkIgnoreMethodCall(String methodCallType, String callerFullMethod, String calleeFullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        methodCallAddData4Type(elHandler, methodCallType, usedVariableMap, displayMap);
        methodCallAddData4CallerMethod(elHandler, callerFullMethod, usedVariableMap, displayMap);
        methodCallAddData4CalleeMethod(elHandler, calleeFullMethod, usedVariableMap, displayMap);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 处理方法调用时解析被调用对象和参数可能的类型与值判断是否跳过方法，通过调用方法判断
     *
     * @param callerFullMethod
     * @return
     */
    public boolean checkIgnoreMethodCallTypeValueCaller(String callerFullMethod) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        methodCallAddData4CallerMethod(elHandler, callerFullMethod, usedVariableMap, displayMap);
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要通过class文件对应指定层级的目录路径判断是否跳过合并当前的jar/war文件
     *
     * @return
     */
    public boolean checkNeedIgnoreJarWarByClassDirPrefix() {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX);
        if (elHandler.checkExpressionTextFixedTrue()) {
            return true;
        }
        if (elHandler.checkVariableNamePrefixWithNumSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL)) {
            return true;
        }
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME)) {
            return true;
        }
        return elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR);
    }

    /**
     * 通过class文件对应指定层级的目录路径判断是否跳过合并当前的jar/war文件
     *
     * @param classDirPrefixMap key class文件目录层级 value class文件目录对应层级的路径前缀，以/作为分隔符，不会以分隔符开头或结尾
     * @return
     */
    public boolean checkIgnoreJarWarByClassDirPrefix(Map<Integer, Set<String>> classDirPrefixMap) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }

        Map<String, Object> displayMap = new HashMap<>();
        for (Map.Entry<Integer, Set<String>> entry : classDirPrefixMap.entrySet()) {
            Integer level = entry.getKey();
            Set<String> classDirPrefixSet = entry.getValue();
            displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + level, classDirPrefixSet);
            if (elHandler.checkVariableNamePrefixWithNumSpecified(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL, level)) {
                usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + level, classDirPrefixSet);
            }
        }
        return elHandler.runExpression(usedVariableMap, displayMap);
    }

    /**
     * 检查是否需要跳过处理XML文件中的Spring Bean
     * @param beanName
     * @param className
     * @param profile
     * @return
     */
    public boolean checkIgnoreSpringBeanInXml(String beanName,String className,String profile) {
        ElHandler elHandler = getElHandlerMap(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML);
        Map<String, Object> usedVariableMap = elHandler.genMap4ElExecute();
        if (usedVariableMap == null) {
            return false;
        }
        Map<String, Object> displayMap = new HashMap<>();
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME.getVariableName(), beanName);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME.getVariableName(), beanName);
        }
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME.getVariableName(), className);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME.getVariableName(),className );
        }
        displayMap.put(JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName(), profile);
        if (elHandler.checkVariableNameSpecified(JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE)) {
            usedVariableMap.put(JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName(), profile);
        }
        return elHandler.runExpression(usedVariableMap, displayMap);
    }
}
