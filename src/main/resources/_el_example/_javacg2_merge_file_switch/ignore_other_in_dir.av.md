# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_merge_file_switch/ignore_other_in_dir.av

- 表达式配置文件作用

在 合并需要解析的目录或jar/war文件时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过合并（及解析）对应的文件

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的文件

指定是否跳过合并目录中的非jar、war、class文件

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_DIR

以下为支持的表达式变量示例

# 2. file_path - 合并目录中的其他文件判断文件绝对路径

- 表达式变量说明

目录中的文件绝对路径

以斜杠/为分隔符

- 表达式示例说明

在合并目录中的其他文件时，判断文件绝对路径是否包含指定关键字，忽略匹配的文件

- 表达式示例文本

```js
string.contains(file_path, '/out/test/resources/')
```

- 表达式示例文本 - 代码中指定

```java
"string.contains(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_PATH_IN_DIR.getVariableName() + ", '/out/test/resources/')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInDirAFPID

# 3. file_dir_path - 合并目录中的其他文件判断文件所在目录绝对路径

- 表达式变量说明

目录中的文件所在目录绝对路径

以斜杠/为分隔符，不以分隔符结尾

- 表达式示例说明

在合并目录中的其他文件时，判断文件所在目录绝对路径是否包含指定关键字，忽略匹配的文件

- 表达式示例文本

```js
string.contains(file_dir_path, '/out/test/resources')
```

- 表达式示例文本 - 代码中指定

```java
"string.contains(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_ABSOLUTE_FILE_DIR_PATH_IN_DIR.getVariableName() + ", '/out/test/resources')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInDirAFDPID

# 4. file_name - 合并目录中的其他文件判断文件名

- 表达式变量说明

文件名称

- 表达式示例说明

在合并目录中的其他文件时，判断文件名是否以指定关键字结尾，忽略匹配的文件

- 表达式示例文本

```js
string.endsWith(file_name, '.xml')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", '.xml')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInDirFN

# 5. file_ext - 合并目录中的其他文件判断文件后缀

- 表达式变量说明

非jar、war、class文件后缀

以.开头

- 表达式示例说明

在合并目录中的其他文件时，判断文件后缀是否等于指定关键字，忽略匹配的文件

- 表达式示例文本

```js
file_ext == '.xml'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName() + " == '.xml'"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInDirOFE

