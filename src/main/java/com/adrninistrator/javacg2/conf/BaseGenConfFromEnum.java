package com.adrninistrator.javacg2.conf;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description: 根据枚举生成配置文件的基类
 */
public abstract class BaseGenConfFromEnum {

    private static final Logger logger = LoggerFactory.getLogger(BaseGenConfFromEnum.class);

    public static final String DIR_RESOURCES = "src/main/resources/";

    private void writeCommentLine(Writer writer, String data) throws IOException {
        writeNewLine(writer, JavaCG2Constants.COMMENT_PROPERTIES + data);
    }

    private void writeAVCommentLine(Writer writer, String data) throws IOException {
        writeNewLine(writer, JavaCG2Constants.COMMENT_AVIATOR + data);
    }

    private void writeNewLine(Writer writer, String data) throws IOException {
        writer.write(data + JavaCG2Constants.NEW_LINE_WINDOWS);
    }

    protected void genMainConfig(MainConfigInterface[] mainConfigs) {
        try (Writer writer = JavaCG2FileUtil.genBufferedWriter(DIR_RESOURCES + mainConfigs[0].getFileName())) {
            for (MainConfigInterface mainConfig : mainConfigs) {
                for (String description : mainConfig.getDescriptions()) {
                    writeCommentLine(writer, description);
                }
                writeNewLine(writer, mainConfig.getKey() + "=" + mainConfig.getDefaultValue());
                writeNewLine(writer, "");
            }
        } catch (Exception e) {
            logger.error("error ", e);
            throw new JavaCG2RuntimeException();
        }
    }

    protected void genOtherConfig(OtherConfigInterface[] otherConfigs, String[] ignoreNames) {
        for (OtherConfigInterface otherConfig : otherConfigs) {
            String enumName = otherConfig.getEnumName();
            boolean skip = false;
            if (ArrayUtils.isNotEmpty(ignoreNames)) {
                for (String ignoreName : ignoreNames) {
                    if (enumName.equals(ignoreName)) {
                        skip = true;
                        break;
                    }
                }
            }
            if (skip) {
                continue;
            }
            try (Writer writer = JavaCG2FileUtil.genBufferedWriter(DIR_RESOURCES + otherConfig.getKey())) {
                for (String description : otherConfig.getDescriptions()) {
                    writeCommentLine(writer, description);
                }
                String[] defaultValues = otherConfig.getDefaultValues();
                if (ArrayUtils.isNotEmpty(defaultValues)) {
                    for (String defaultValue : defaultValues) {
                        writeNewLine(writer, defaultValue);
                    }
                }
            } catch (Exception e) {
                logger.error("error ", e);
                throw new JavaCG2RuntimeException();
            }
        }
    }

    protected void genElConfig(ElConfigInterface[] elConfigs, Map<String, String[]> fileUsageMap) {
        String elExampleName = "";
        for (ElConfigInterface elConfig : elConfigs) {
            String fileName = elConfig.getKey();
            if (fileName.endsWith(JavaCG2Constants.EXT_MD)) {
                elExampleName = fileName;
            }
        }
        for (ElConfigInterface elConfig : elConfigs) {
            String fileName = elConfig.getKey();
            boolean elExample = fileName.endsWith(JavaCG2Constants.EXT_MD);
            File file = new File(DIR_RESOURCES + elConfig.getKey());
            if (!JavaCG2FileUtil.isDirectoryExists(file.getParent(), true)) {
                throw new JavaCG2RuntimeException("创建目录失败");
            }
            try (Writer writer = JavaCG2FileUtil.genBufferedWriter(file.getAbsolutePath())) {
                String[] usages = null;
                for (Map.Entry<String, String[]> entry : fileUsageMap.entrySet()) {
                    if (fileName.startsWith(entry.getKey() + "/")) {
                        usages = entry.getValue();
                        break;
                    }
                }
                if (usages != null) {
                    for (String usage : usages) {
                        writeAVCommentLine(writer, "（作用）" + usage);
                    }
                } else if (!elExample) {
                    throw new JavaCG2RuntimeException("未设置当前文件对应的作用 " + fileName);
                }

                if (elExample) {
                    String[] elExampleDescriptions = genElExampleDescriptions();
                    for (String elExampleDescription : elExampleDescriptions) {
                        writeNewLine(writer, elExampleDescription);
                    }
                } else {
                    for (String description : elConfig.getDescriptions()) {
                        writeAVCommentLine(writer, "（范围）" + description);
                    }
                    writeAVCommentLine(writer, "（表达式语言使用示例文件）请参考 " + elExampleName);
                    writeAVCommentLine(writer, "（允许使用的变量）{" + StringUtils.joinWith("} {", "变量名称", "变量类型", "变量描述", "变量示例") + "}");
                    for (ElAllowedVariableInterface elAllowedVariable : elConfig.getElAllowedVariableEnums()) {
                        String descriptions = StringUtils.join(elAllowedVariable.getDescriptions(), ", ");
                        String valueExamples = StringUtils.join(elAllowedVariable.getValueExamples(), ", ");
                        writeAVCommentLine(writer, "{" + StringUtils.joinWith("} {", elAllowedVariable.getVariableName(), elAllowedVariable.getType(), descriptions,
                                valueExamples) + "}");
                    }
                }
            } catch (Exception e) {
                logger.error("error ", e);
                throw new JavaCG2RuntimeException();
            }
        }
    }

    private String[] genElExampleDescriptions() {
        return new String[]{
                "# 表达式语言配置文件说明",
                "当前文件为表达式语言示例配置文件，使用 aviator 表达式语言组件，语法与 Java 类似",
                "使用文档可参考 https://www.yuque.com/boyan-avfmj/aviatorscript",
                "每个配置文件的表达式语言的执行结果类型需要为 boolean ，即结果要么是 true ，要么是 false",
                "通过表达式语言的执行结果，决定配置文件所对应场景下执行什么操作",
                "配置文件中有说明允许使用的变量信息",
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 表达式语言示例",
                chooseElExampleText(),
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 表达式语言语法 - aviator 默认支持",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 字符串处理",
                "除判断字符串是否等于指定值外，需要使用 aviator 提供的 string.xxx() 函数对字符串进行判断",
                "字符串常量可以使用单引号包含，如 'abc'",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### ==",
                "（作用）判断字符串类型的变量是否等于指定内容",
                "（语法）{字符串类型变量名称} == {常量字符串}",
                "（示例）str1 == 'abc'",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.startsWith()",
                "（作用）判断字符串类型的变量是否以指定内容开头",
                "（语法）string.startsWith({字符串类型变量名称}, {常量字符串})",
                "（示例）string.startsWith(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.endsWith()",
                "（作用）判断字符串类型的变量是否以指定内容结尾",
                "（语法）string.endsWith({字符串类型变量名称}, {常量字符串})",
                "（示例）string.endsWith(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.contains()",
                "（作用）判断字符串类型的变量是否包含指定内容",
                "（语法）string.contains({字符串类型变量名称}, {常量字符串})",
                "（示例）string.contains(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.length()",
                "（作用）获取字符串类型的变量的长度",
                "（语法）string.length({字符串类型变量名称})",
                "（示例）string.length(str1)",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 整型处理",
                "整形的判断与 Java 语法相同，可使用比较运算符：==、<、>、<=、>=、!=",
                "（语法）{整型变量名称} {比较运算符} {常量整形值}",
                "（示例）int1 == 1",
                "（示例）int1 != 1",
                "（示例）int1 >= 1",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 逻辑判断",
                "aviator 支持的逻辑判断运算符与 Java 相同",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### &&",
                "（作用）判断两个条件是否同时成立，只有两个条件都为 true 时，整体结果才为 true",
                "（语法）{条件1} && {条件2}",
                "（示例）x > 10 && y < 20",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### ||",
                "（作用）判断两个条件中是否有一个成立，只要有一个条件为 true，整体结果就为 true",
                "（语法）{条件1} || {条件2}",
                "（示例）x > 10 || y < 20",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### !",
                "（作用）取反运算符，用于将条件的结果取反，若条件为 true，则结果为 false；若条件为 false，则结果为 true",
                "（语法）!{条件}",
                "（示例）!(x > 10)",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### ()",
                "（作用）用于改变运算顺序，确保按照期望的顺序执行多个逻辑表达式",
                "（语法）({表达式1} {运算符} {表达式2})",
                "（示例）(x > 10 && y < 20) || z == 5",
        };
    }

    protected abstract String chooseElExampleText();
}
