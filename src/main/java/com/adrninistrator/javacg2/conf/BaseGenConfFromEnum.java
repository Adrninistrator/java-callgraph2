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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
            String enumName = otherConfig.getEnumConstantsName();
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
                    writeAVCommentLine(writer, "（表达式使用示例文件）请参考 " + elExampleName);
                    writeAVCommentLine(writer, "（允许使用的变量）{" + StringUtils.joinWith("} {", "变量名称", "变量类型", "变量描述", "变量值示例") + "}");
                    Set<String> allowedVariableNameSet = new HashSet<>();
                    for (ElAllowedVariableInterface elAllowedVariable : elConfig.getElAllowedVariableEnums()) {
                        if (!allowedVariableNameSet.add(elAllowedVariable.getVariableName())) {
                            throw new JavaCG2RuntimeException("存在重复的变量名称 " + elAllowedVariable.getVariableName());
                        }
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
                "# 表达式配置文件说明",
                "当前文件为表达式示例配置文件，使用 aviator 表达式组件，语法与 Java 类似",
                "使用文档可参考 https://www.yuque.com/boyan-avfmj/aviatorscript",
                "每个配置文件的表达式的执行结果类型需要为 boolean ，即结果要么是 true ，要么是 false",
                "通过表达式的执行结果，决定配置文件所对应场景下执行什么操作",
                "配置文件中有说明允许使用的变量信息",
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 查看表达式忽略的数据",
                "若表达式用于忽略数据，则被忽略的数据会记录在日志文件中，保存在当前输出目录中，文件名为 " + JavaCG2Constants.EL_IGNORE_DATA_FILE_NAME,
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 表达式示例",
                chooseElExampleText(),
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 表达式调试方式",
                chooseElDebugModeText(),
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 表达式语法 - aviator 默认支持",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 返回固定值",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### true",
                "若表达式配置为“true”，则表达式执行结果固定为 true",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### false",
                "若表达式配置为“false”，或未指定表达式，则表达式执行结果固定为 false",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 字符串处理",
                "除判断字符串是否等于指定值外，需要使用 aviator 提供的 string.xxx() 函数对字符串进行判断",
                "字符串常量可以使用单引号包含，如 'abc'",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### +",
                "（作用）拼接字符串",
                "（语法）{字符串变量/常量/运算结果} + {字符串变量/常量/运算结果} + ...",
                "（示例）str1 + 'abc'",
                "（示例）'abc' + '123'",
                "（示例）str1 + str2",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### ==",
                "（作用）判断字符串类型的变量是否等于指定内容",
                "（语法）{字符串变量/常量/运算结果} == {字符串变量/常量/运算结果}",
                "（示例）str1 == 'abc'",
                "（示例）str1 == str2 + 'abc'",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.startsWith()",
                "（作用）判断字符串类型的变量是否以指定内容开头",
                "（语法）string.startsWith({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.startsWith(str1, 'abc')",
                "（示例）string.startsWith(str1, str2 + 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.endsWith()",
                "（作用）判断字符串类型的变量是否以指定内容结尾",
                "（语法）string.endsWith({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.endsWith(str1, 'abc')",
                "（示例）string.endsWith(str1, str2 + 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.contains()",
                "（作用）判断字符串类型的变量是否包含指定内容",
                "（语法）string.contains({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.contains(str1, 'abc')",
                "（示例）string.contains(str1, str2 + 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.length()",
                "（作用）获取字符串类型的变量的长度",
                "（语法）string.length({字符串变量/常量/运算结果})",
                "（示例）string.length(str1)",
                "（示例）string.length(str1 + str2)",
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
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 集合处理",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### include",
                "（作用）判断集合中是否包含指定的元素",
                "（语法）include({集合变量}, {字符串/整型等常量})",
                "（示例）include(set1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### !include",
                "（作用）判断集合中是否不包含指定的元素",
                "（语法）!include({集合变量}, {字符串/整型等常量})",
                "（示例）!include(set1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "# 表达式语法 - java-callgraph2 组件扩展支持",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 比较字符串忽略大小写的方法",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.containsIC()",
                "（作用）判断字符串类型的变量是否包含指定内容，忽略大小写",
                "（语法）string.containsIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.containsIC(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.endsWithIC()",
                "（作用）判断字符串类型的变量是否以指定内容结尾，忽略大小写",
                "（语法）string.endsWithIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.endsWithIC(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.equalsIC()",
                "（作用）判断字符串类型的变量是否与指定内容相等，忽略大小写",
                "（语法）string.equalsIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.equalsIC(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.startsWithIC()",
                "（作用）判断字符串类型的变量是否以指定内容开头，忽略大小写",
                "（语法）string.startsWithIC({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果})",
                "（示例）string.startsWithIC(str1, 'abc')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "## 比较字符串支持比较多个的方法",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.containsAny()",
                "（作用）判断字符串类型的变量是否包含多个指定内容中的任意一个",
                "（语法）string.containsAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)",
                "（示例）string.containsAny(str1, 'abc', 'def', 'ghi')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.endsWithAny()",
                "（作用）判断字符串类型的变量是否以多个指定内容中的任意一个结尾",
                "（语法）string.endsWithAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)",
                "（示例）string.endsWithAny(str1, 'abc', 'def', 'ghi')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.equalsAny()",
                "（作用）判断字符串类型的变量是否与多个指定内容中的任意一个相等",
                "（语法）string.equalsAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)",
                "（示例）string.equalsAny(str1, 'abc', 'def', 'ghi')",
                JavaCG2Constants.NEW_LINE_WINDOWS + "### string.startsWithAny()",
                "（作用）判断字符串类型的变量是否以多个指定内容中的任意一个开头",
                "（语法）string.startsWithAny({字符串变量/常量/运算结果}, {字符串变量/常量/运算结果1}, {字符串变量/常量/运算结果2}, ...)",
                "（示例）string.startsWithAny(str1, 'abc', 'def', 'ghi')",
        };
    }

    protected abstract String chooseElExampleText();

    protected abstract String chooseElDebugModeText();
}
