# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_parse_class_method_switch/parse_ignore_method.av

- 表达式配置文件作用

在 解析类或方法时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过解析对应的类或方法

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的类或方法

指定是否跳过解析方法

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD

以下为支持的表达式变量示例

# 2. class_name - 解析方法判断类名

- 表达式变量说明

完整类名

- 表达式示例说明

在解析方法时，判断类名是否以指定关键字开头，仅处理匹配的方法

- 表达式示例文本

```js
!string.startsWith(class_name, 'test.callgraph.annotation.')
```

- 表达式示例文本 - 代码中指定

```java
"!string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.')"
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreMethodClassName

# 3. package_name - 解析方法判断类的包名

- 表达式变量说明

完整包名

不会以.结束

- 表达式示例说明

在解析方法时，判断类的包名是否等于指定关键字，仅处理匹配的方法

- 表达式示例文本

```js
package_name != 'test.callgraph.annotation'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreMethodPackageName

# 4. simple_class_name - 解析方法判断简单类名

- 表达式变量说明

简单类名

- 表达式示例说明

在解析方法时，判断简单类名是否等于指定关键字，仅处理匹配的方法

- 表达式示例文本

```js
simple_class_name != 'ArgCalleeTypeService1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'"
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreMethodSimpleClassName

# 5. method_name - 解析方法判断方法名

- 表达式变量说明

方法名

不包括括号及方法参数

- 表达式示例说明

在解析方法时，判断方法名是否等于指定关键字，仅处理匹配的方法

- 表达式示例文本

```js
method_name != '<init>'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_NAME.getVariableName() + " != '" + "<init>" + "'"
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreMethodMethodName

# 6. method_arg_num - 解析方法判断方法参数数量

- 表达式变量说明

方法参数数量

- 表达式示例说明

在解析方法时，判断方法参数数量是否等于指定值，仅处理匹配的方法

- 表达式示例文本

```js
method_arg_num != 4
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_METHOD_ARG_NUM.getVariableName() + " != 4"
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreMethodMethodArgNum

# 7. full_method - 解析方法判断完整方法

- 表达式变量说明

完整方法

- 表达式示例说明

在解析方法时，判断完整方法是否等于指定关键字，仅处理匹配的方法

- 表达式示例文本

```js
full_method != 'test.callgraph.implement.AbstractClass1:test()'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_FULL_METHOD.getVariableName() + " != 'test.callgraph.implement.AbstractClass1:test()'"
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreMethodFullMethod

