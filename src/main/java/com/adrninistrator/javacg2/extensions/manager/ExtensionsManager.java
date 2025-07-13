package com.adrninistrator.javacg2.extensions.manager;

import com.adrninistrator.javacg2.dto.inputoutput.JavaCG2InputAndOutput;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.extensions.annotationattributes.AnnotationAttributesFormatterInterface;
import com.adrninistrator.javacg2.extensions.annotationattributes.DefaultAnnotationAttributesFormatter;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.extensions.codeparser.CodeParserInterface;
import com.adrninistrator.javacg2.extensions.codeparser.JarEntryOtherFileParser;
import com.adrninistrator.javacg2.extensions.codeparser.MethodAnnotationParser;
import com.adrninistrator.javacg2.extensions.codeparser.SpringXmlBeanParserInterface;
import com.adrninistrator.javacg2.extensions.methodcall.JavaCG2MethodCallExtensionInterface;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/6
 * @description: 扩展类管理类
 */
public class ExtensionsManager {

    private static final Logger logger = LoggerFactory.getLogger(ExtensionsManager.class);

    // 保存所有的代码解析扩展类列表
    private final List<CodeParserInterface> allCodeParserList = new ArrayList<>();

    /*
        保存处理jar包中其他类型文件的扩展类
        key:
            对应的文件类型小写形式（包含"."，即格式为".xxx"）
        value:
            当前文件类型对应的扩展类列表（一种文件类型可能会有多个对应的扩展类）
     */
    private final Map<String, List<JarEntryOtherFileParser>> otherFileParserMap = new HashMap<>();

    /*
        保存处理方法注解的扩展类
        key:
            对应的注解类名
        value:
            当前注解类名对应的扩展类（一种文件类型只会有一个对应的扩展类）
     */
    private final Map<String, MethodAnnotationParser> methodAnnotationParserMap = new HashMap<>();

    // 保存方法调用处理扩展类的列表
    private final List<JavaCG2MethodCallExtensionInterface> methodCallExtensionList = new ArrayList<>();

    // 解析并将结果保存在文件的类列表
    private final List<AbstractSaveData2FileParser> saveData2FileParserList = new ArrayList<>();

    /*
        解析并将结果保存在文件的类Map
        key 生成的文件名
        value   对应的类名
     */
    private final Map<String, String> saveData2FileParserNameMap = new HashMap<>();

    private JavaCG2InputAndOutput javaCG2InputAndOutput;

    // 对注解属性的元素值进行格式化的类
    private AnnotationAttributesFormatterInterface annotationAttributesFormatter;

    // Spring XML Bean信息解析类
    private SpringXmlBeanParserInterface springXmlBeanParser;

    private boolean inited = false;

    // 初始化
    public boolean init() {
        if (inited) {
            throw new JavaCG2RuntimeException("不允许重复初始化");
        }

        // 固定添加Spring XML Bean信息解析类
        if (springXmlBeanParser != null) {
            allCodeParserList.add(springXmlBeanParser);
        }
        JavaCG2OutputInfo javaCG2OutputInfo = javaCG2InputAndOutput.getJavaCG2OutputInfo();
        for (CodeParserInterface codeParser : allCodeParserList) {
            // 初始化扩展类
            codeParser.initCodeParser();

            if (codeParser instanceof JarEntryOtherFileParser) {
                // 处理jar包中其他类型文件的扩展类
                JarEntryOtherFileParser jarEntryOtherFileParser = (JarEntryOtherFileParser) codeParser;
                String[] otherFileExtensions = jarEntryOtherFileParser.chooseJarEntryOtherFileExt();
                if (otherFileExtensions != null) {
                    for (String otherFileExtension : otherFileExtensions) {
                        List<JarEntryOtherFileParser> otherFileParserList = otherFileParserMap.computeIfAbsent(otherFileExtension, k -> new ArrayList<>());
                        otherFileParserList.add(jarEntryOtherFileParser);
                    }
                }

                // 处理解析并将结果保存在文件的类
                if (codeParser instanceof AbstractSaveData2FileParser) {
                    AbstractSaveData2FileParser saveData2FileParser = (AbstractSaveData2FileParser) codeParser;
                    if (!saveData2FileParser.init(javaCG2OutputInfo.getOutputDirPath(), javaCG2OutputInfo.getOutputFileExt())) {
                        logger.error("初始化失败 {}", codeParser.getClass().getName());
                        return false;
                    }
                    // 增加其他文件信息
                    String outputFileName = saveData2FileParser.chooseFileName();
                    String[] outputFileNames;
                    if (outputFileName != null) {
                        outputFileNames = new String[]{outputFileName};
                    } else {
                        outputFileNames = saveData2FileParser.chooseFileNames();
                    }
                    for (String tmpOutputFileName : outputFileNames) {
                        String existedAbstractSaveData2FileParserName = saveData2FileParserNameMap.get(tmpOutputFileName);
                        if (existedAbstractSaveData2FileParserName != null) {
                            logger.error("出现不同的 {} 实现类使用了相同的生成文件名 {} {} {}", AbstractSaveData2FileParser.class.getName(), tmpOutputFileName,
                                    saveData2FileParser.getClass().getName(), existedAbstractSaveData2FileParserName);
                            return false;
                        }
                        javaCG2OutputInfo.addOtherFileInfo(tmpOutputFileName);
                        saveData2FileParserNameMap.put(tmpOutputFileName, saveData2FileParser.getClass().getName());
                    }
                    saveData2FileParserList.add(saveData2FileParser);
                }
            } else if (codeParser instanceof MethodAnnotationParser) {
                // 处理方法注解的扩展类
                MethodAnnotationParser methodAnnotationParser = (MethodAnnotationParser) codeParser;
                String[] methodAnnotationClassNames = methodAnnotationParser.chooseMethodAnnotationClassName();
                if (methodAnnotationClassNames != null) {
                    for (String methodAnnotationClass : methodAnnotationClassNames) {
                        methodAnnotationParserMap.put(methodAnnotationClass, methodAnnotationParser);
                    }
                }
            } else {
                logger.error("不支持的类型 {}", codeParser.getClass().getName());
                return false;
            }
        }

        inited = true;
        return true;
    }

    // 关闭处理
    public void close() {
        for (AbstractSaveData2FileParser saveData2FileParser : saveData2FileParserList) {
            saveData2FileParser.close();
        }
    }

    private void checkInited() {
        if (!inited) {
            throw new JavaCG2RuntimeException("还未完成初始化");
        }
    }

    /**
     * 获得所有代码解析扩展类名列表
     *
     * @return
     */
    public List<String> getAllCodeParserNameList() {
        List<String> allCodeParserNameList = new ArrayList<>();
        for (CodeParserInterface codeParserInterface : allCodeParserList) {
            allCodeParserNameList.add(codeParserInterface.getClass().getName());
        }
        return allCodeParserNameList;
    }

    public List<JarEntryOtherFileParser> getJarEntryOtherFileParserList(String fileExtension) {
        checkInited();
        if (StringUtils.isBlank(fileExtension)) {
            return null;
        }
        return otherFileParserMap.get(fileExtension);
    }

    public MethodAnnotationParser getMethodAnnotationParser(String methodAnnotationClass) {
        checkInited();
        if (StringUtils.isBlank(methodAnnotationClass)) {
            return null;
        }
        return methodAnnotationParserMap.get(methodAnnotationClass);
    }

    public AnnotationAttributesFormatterInterface getAnnotationAttributesFormatter() {
        if (annotationAttributesFormatter == null) {
            annotationAttributesFormatter = new DefaultAnnotationAttributesFormatter();
        }

        return annotationAttributesFormatter;
    }

    /**
     * 添加代码解析扩展类
     * 需要在调用run()方法之前调用当前方法
     *
     * @param codeParser
     */
    public void addCodeParser(CodeParserInterface codeParser) {
        allCodeParserList.add(codeParser);
    }

    /**
     * 添加方法调用处理扩展类
     *
     * @param methodCallExtension
     */
    public void addJavaCG2MethodCallExtension(JavaCG2MethodCallExtensionInterface methodCallExtension) {
        methodCallExtensionList.add(methodCallExtension);
    }

    //
    public List<JavaCG2MethodCallExtensionInterface> getMethodCallExtensionList() {
        return methodCallExtensionList;
    }

    public SpringXmlBeanParserInterface getSpringXmlBeanParser() {
        return springXmlBeanParser;
    }

    public void setSpringXmlBeanParser(SpringXmlBeanParserInterface springXmlBeanParser) {
        this.springXmlBeanParser = springXmlBeanParser;
    }

    /**
     * 设置注解属性格式化类
     *
     * @param annotationAttributesFormatter
     */
    public void setAnnotationAttributesFormatter(AnnotationAttributesFormatterInterface annotationAttributesFormatter) {
        this.annotationAttributesFormatter = annotationAttributesFormatter;
    }

    public void setJavaCG2InputAndOutput(JavaCG2InputAndOutput javaCG2InputAndOutput) {
        this.javaCG2InputAndOutput = javaCG2InputAndOutput;
    }
}
