# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_parse_method_call_switch/parse_ignore_method_call_type_value_caller.av

- 表达式配置文件作用

在 解析方法调用时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过解析对应的方法调用

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法调用

指定解析方法调用被调用对象和参数可能的类型与值需要跳过哪些方法，通过调用方法判断

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER

以下为支持的表达式变量示例

# 2. er_class_name - 方法调用类型与值判断调用类名

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在解析方法调用被调用对象和参数可能的类型与值时，判断调用类名是否等于指定关键字，仅处理匹配的调用方法

- 表达式示例文本

```js
er_class_name != 'test.callgraph.annotation.CallMethodWithAnnotation'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
);
```

- 表达式示例类名

test.el.methodcalltypevalue.TestElParseIgnoreMethodCallTypeValueErClassName

# 3. er_package_name - 方法调用类型与值判断调用类包名

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在解析方法调用被调用对象和参数可能的类型与值时，判断调用类包名是否等于指定关键字，仅处理匹配的调用方法

- 表达式示例文本

```js
er_package_name != 'test.callgraph.annotation'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
);
```

- 表达式示例类名

test.el.methodcalltypevalue.TestElParseIgnoreMethodCallTypeValueErPackageName

# 4. er_simple_class_name - 方法调用类型与值判断调用简单类名

- 表达式变量说明

调用方简单类名

- 表达式示例说明

在解析方法调用被调用对象和参数可能的类型与值时，判断调用简单类名是否等于指定关键字，仅处理匹配的调用方法

- 表达式示例文本

```js
er_simple_class_name != 'ArgCalleeTypeService1'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'"
);
```

- 表达式示例类名

test.el.methodcalltypevalue.TestElParseIgnoreMethodCallTypeValueErSimpleClassName

# 5. er_method_name - 方法调用类型与值判断调用方法名

- 表达式变量说明

调用方方法名

不包括括号及方法参数

- 表达式示例说明

在解析方法调用被调用对象和参数可能的类型与值时，判断调用方法名是否等于指定关键字，仅处理匹配的调用方法

- 表达式示例文本

```js
er_method_name != 'test5'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName() + " != 'test5'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName() + " != 'test5'"
);
```

- 表达式示例类名

test.el.methodcalltypevalue.TestElParseIgnoreMethodCallTypeValueErMethodName

# 6. er_method_arg_num - 方法调用类型与值判断调用方法参数数量

- 表达式变量说明

调用方方法参数数量

- 表达式示例说明

在解析方法调用被调用对象和参数可能的类型与值时，判断调用方法参数数量是否等于指定值，仅处理匹配的调用方法

- 表达式示例文本

```js
er_method_arg_num != 5
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName() + " != 5"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName() + " != 5"
);
```

- 表达式示例类名

test.el.methodcalltypevalue.TestElParseIgnoreMethodCallTypeValueErMethodArgNum

# 7. er_full_method - 方法调用类型与值判断调用完整方法

- 表达式变量说明

调用方完整方法

包括括号及方法参数

- 表达式示例说明

在解析方法调用被调用对象和参数可能的类型与值时，判断调用完整方法是否等于指定关键字，仅处理匹配的调用方法

- 表达式示例文本

```js
er_full_method != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL_TYPE_VALUE_CALLER, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'"
);
```

- 表达式示例类名

test.el.methodcalltypevalue.TestElParseIgnoreMethodCallTypeValueErFullMethod

