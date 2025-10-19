# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_merge_file_switch/ignore_other_in_jar_war.av

- 表达式配置文件作用

在 合并需要解析的目录或jar/war文件时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过合并（及解析）对应的文件

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的文件

指定是否跳过合并jar、war中的非jar、war、class文件

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR

以下为支持的表达式变量示例

# 2. file_path - 合并jar文件中的其他文件判断文件相对路径

- 表达式变量说明

jar/war文件中的文件相对路径

相对根目录的路径

以斜杠/为分隔符，不以分隔符开头

- 表达式示例说明

在合并jar文件中的其他文件时，判断文件相对路径是否以指定关键字开头，仅处理匹配的文件

- 表达式示例文本

```js
!string.startsWith(file_path, 'META-INF/')
```

- 表达式示例文本 - 代码中指定

```java
"!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR.getVariableName() + ", 'META-INF/')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInJarFPIJW

# 3. file_path - 合并war文件中的其他文件判断文件相对路径

- 表达式变量说明

jar/war文件中的文件相对路径

相对根目录的路径

以斜杠/为分隔符，不以分隔符开头

- 表达式示例说明

在合并war文件中的其他文件时，判断文件相对路径是否以指定关键字开头，仅处理匹配的文件

- 表达式示例文本

```js
!string.startsWith(file_path, 'META-INF/')
```

- 表达式示例文本 - 代码中指定

```java
"!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_PATH_IN_JAR_WAR.getVariableName() + ", 'META-INF/')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInWarFPIJW

# 4. file_dir_path - 合并jar文件中的其他文件判断文件所在目录相对路径

- 表达式变量说明

jar/war文件中的文件所在目录相对路径

相对根目录的路径

以斜杠/为分隔符，不以分隔符开头或结尾

- 表达式示例说明

在合并jar文件中的其他文件文件时，判断文件所在目录相对路径（相对根目录）是否等于指定关键字，仅处理匹配的文件

- 表达式示例文本

```js
file_dir_path != 'META-INF'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + " != 'META-INF'"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInJarFDPIJW

# 5. file_dir_path - 合并war文件中的其他文件判断文件所在目录相对路径

- 表达式变量说明

jar/war文件中的文件所在目录相对路径

相对根目录的路径

以斜杠/为分隔符，不以分隔符开头或结尾

- 表达式示例说明

在合并war文件中的其他文件文件时，判断文件所在目录相对路径（相对根目录）是否等于指定关键字，仅处理匹配的文件

- 表达式示例文本

```js
file_dir_path != 'META-INF'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_DIR_PATH_IN_JAR_WAR.getVariableName() + " != 'META-INF'"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInWarFDPIJW

# 6. file_name - 合并jar文件中的其他文件判断文件名

- 表达式变量说明

文件名称

- 表达式示例说明

在合并jar文件中的其他文件时，判断文件名是否以指定关键字结尾，仅处理匹配的文件

- 表达式示例文本

```js
!string.endsWith(file_name, '.MF')
```

- 表达式示例文本 - 代码中指定

```java
"!string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", '.MF')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInJarFN

# 7. file_name - 合并war文件中的其他文件判断文件名

- 表达式变量说明

文件名称

- 表达式示例说明

在合并war文件中的其他文件时，判断文件名是否以指定关键字结尾，仅处理匹配的文件

- 表达式示例文本

```js
!string.endsWith(file_name, '.MF')
```

- 表达式示例文本 - 代码中指定

```java
"!string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_FILE_NAME.getVariableName() + ", '.MF')"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInWarFN

# 8. file_ext - 合并jar文件中的其他文件判断文件后缀

- 表达式变量说明

非jar、war、class文件后缀

以.开头

- 表达式示例说明

在合并jar文件中的其他文件时，判断文件后缀是否等于指定关键字，仅处理匹配的文件

- 表达式示例文本

```js
file_ext != '.MF'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName() + " != '.MF'"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInJarOFE

# 9. file_ext - 合并war文件中的其他文件判断文件后缀

- 表达式变量说明

非jar、war、class文件后缀

以.开头

- 表达式示例说明

在合并war文件中的其他文件时，判断文件后缀是否等于指定关键字，仅处理匹配的文件

- 表达式示例文本

```js
file_ext != '.MF'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_MF_OTHER_FILE_EXT.getVariableName() + " != '.MF'"
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreOtherInWarOFE

