# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_merge_file_switch/ignore_jar_war_by_class_dir_prefix.av

- 表达式配置文件作用

在 合并需要解析的目录或jar/war文件时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过合并（及解析）对应的文件

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的文件

通过class文件对应指定层级的目录路径判断是否跳过合并当前jar、war文件

相当于通过jar、war文件中类的包名控制是否跳过合并当前jar、war文件

以下参数为jar、war文件中的class文件对应指定层级的目录路径集合。在表达式中可通过“include”方法判断集合中是否包含指定元素

集合中的元素类型为字符串，以/作为分隔符，不会以分隔符开头或结尾

例如jar文件中有a1/c1.class、a2/b2/c2.class，则该jar文件中的class文件目录1级路径有a1、a2，2级路径有a2/b2，没有层级大于2级的路径

在使用以下 class_dir_prefix_level_ 参数时，需要以 class_dir_prefix_level_ 开头，后续通过数字指定class文件路径层级

例如 class_dir_prefix_level_3 代表第3级class文件路径集合

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX

以下为支持的表达式变量示例

# 2. class_dir_prefix_level_ - 合并jar文件判断所有class文件所在指定层级的目录路径

- 表达式变量说明

jar/war文件中的class文件对应指定层级的目录路径集合

- 表达式示例说明

在合并jar文件时，判断所有class文件所在指定层级的目录路径是否包括指定的路径，仅处理class文件所在指定层级的目录路径包括特定路径的jar文件

- 表达式示例文本

```js
!include(class_dir_prefix_level_2, 'com/adrninistrator') && !include(class_dir_prefix_level_3, 'org/apache/bcel')
```

- 表达式示例文本 - 代码中指定

```java
"!include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "2, 'com/adrninistrator') && !include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "3, 'org/apache/bcel')"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX, 
    "!include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "2, 'com/adrninistrator') && !include(" + JavaCG2ElAllowedVariableEnum.EAVE_MF_CLASS_DIR_PREFIX_LEVEL.getVariableName() + "3, 'org/apache/bcel')"
);
```

- 表达式示例类名

test.el.mergefile.TestElMergeFileIgnoreJarByClassDirPrefix

