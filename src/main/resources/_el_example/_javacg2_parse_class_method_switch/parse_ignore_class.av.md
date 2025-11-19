# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_parse_class_method_switch/parse_ignore_class.av

- 表达式配置文件作用

在 解析类或方法时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过解析对应的类或方法

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的类或方法

指定是否跳过解析类

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS

以下为支持的表达式变量示例

# 2. class_name - 解析类判断类名等于关键字

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否等于指定关键字，仅处理匹配的类

- 表达式示例文本

```js
class_name != 'test.callgraph.annotation.CallMethodWithAnnotation'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS, 
    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
);
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreClassClassName

# 3. package_name - 解析类判断包名

- 表达式变量说明

完整包名

不会以.结束

- 表达式示例说明

在解析类时，判断包名是否等于指定关键字，仅处理匹配的类

- 表达式示例文本

```js
package_name != 'test.callgraph.annotation'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS, 
    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
);
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreClassPackageName

# 4. simple_class_name - 解析类判断简单类名

- 表达式变量说明

简单类名

- 表达式示例说明

在解析类时，判断简单类名是否等于指定关键字，仅处理匹配的类

- 表达式示例文本

```js
simple_class_name != 'ArgCalleeTypeService1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_CLASS, 
    JavaCG2ElAllowedVariableEnum.EAVE_PARSE_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'"
);
```

- 表达式示例类名

test.el.classmethod.TestElParseIgnoreClassSimpleClassName

