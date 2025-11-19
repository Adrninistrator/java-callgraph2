# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_parse_method_call_switch/parse_ignore_method_call.av

- 表达式配置文件作用

在 解析方法调用时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过解析对应的方法调用

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的方法调用

指定是否跳过解析方法调用，支持通过方法调用类型、调用方法或被调用方法等判断

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL

以下为支持的表达式变量示例

# 2. method_call_type - 方法调用判断调用类型

- 表达式变量说明

方法调用类型

参考 JavaCG2CallTypeEnum 类

- 表达式示例说明

在解析方法调用时，判断调用类型是否等于指定关键字，仅处理匹配的方法调用

- 表达式示例文本

```js
method_call_type != '_SPR_ACT_C'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName() + " != '" + JavaCG2CallTypeEnum.CTE_SPRING_BEAN_ACTUAL_CLASS.getType() + "'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_METHOD_CALL_TYPE.getVariableName() + " != '" + JavaCG2CallTypeEnum.CTE_SPRING_BEAN_ACTUAL_CLASS.getType() + "'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallMethodCallType

# 3. er_class_name - 方法调用判断调用类名

- 表达式变量说明

调用方完整类名

- 表达式示例说明

在解析方法调用时，判断调用类名是否等于指定关键字，仅处理匹配的方法调用

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
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallErClassName

# 4. er_package_name - 方法调用判断调用类包名

- 表达式变量说明

调用方完整包名

不会以.结束

- 表达式示例说明

在解析方法调用时，判断调用类包名是否等于指定关键字，仅处理匹配的方法调用

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
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallErPackageName

# 5. er_simple_class_name - 方法调用判断调用简单类名

- 表达式变量说明

调用方简单类名

- 表达式示例说明

在解析方法调用时，判断调用简单类名是否等于指定关键字，仅处理匹配的方法调用

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
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_SIMPLE_CLASS_NAME.getVariableName() + " != 'ArgCalleeTypeService1'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallErSimpleClassName

# 6. er_method_name - 方法调用判断调用方法名

- 表达式变量说明

调用方方法名

不包括括号及方法参数

- 表达式示例说明

在解析方法调用时，判断调用方法名是否等于指定关键字，仅处理匹配的方法调用

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
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_NAME.getVariableName() + " != 'test5'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallErMethodName

# 7. er_method_arg_num - 方法调用判断调用方法参数数量

- 表达式变量说明

调用方方法参数数量

- 表达式示例说明

在解析方法调用时，判断调用方法参数数量是否等于指定值，仅处理匹配的方法调用

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
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_METHOD_ARG_NUM.getVariableName() + " != 5"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallErMethodArgNum

# 8. er_full_method - 方法调用判断调用完整方法

- 表达式变量说明

调用方完整方法

包括括号及方法参数

- 表达式示例说明

在解析方法调用时，判断调用完整方法是否等于指定关键字，仅处理匹配的方法调用

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
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_ER_FULL_METHOD.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallErFullMethod

# 9. ee_class_name - 方法调用判断被调用类名

- 表达式变量说明

被调用方完整类名

- 表达式示例说明

在解析方法调用时，判断被调用类名是否等于指定关键字，仅处理匹配的方法调用

- 表达式示例文本

```js
ee_class_name != 'test.callgraph.annotation.CallMethodWithAnnotation'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_CLASS_NAME.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallEeClassName

# 10. ee_package_name - 方法调用判断被调用类包名

- 表达式变量说明

被调用方完整包名

不会以.结束

- 表达式示例说明

在解析方法调用时，判断被调用类包名是否等于指定关键字，仅处理匹配的方法调用

- 表达式示例文本

```js
ee_package_name != 'test.callgraph.annotation'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_PACKAGE_NAME.getVariableName() + " != 'test.callgraph.annotation'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallEePackageName

# 11. ee_simple_class_name - 方法调用判断被调用简单类名

- 表达式变量说明

被调用方简单类名

- 表达式示例说明

在解析方法调用时，判断被调用简单类名是否等于指定关键字，仅处理匹配的方法调用

- 表达式示例文本

```js
ee_simple_class_name != 'Object'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName() + " != '" + java.lang.Object.class.getSimpleName() + "'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_SIMPLE_CLASS_NAME.getVariableName() + " != '" + java.lang.Object.class.getSimpleName() + "'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallEeSimpleClassName

# 12. ee_method_name - 方法调用判断被调用方法名

- 表达式变量说明

被调用方方法名

不包括括号及方法参数

- 表达式示例说明

在解析方法调用时，判断被调用方法名是否等于指定关键字，仅处理匹配的方法调用

- 表达式示例文本

```js
ee_method_name != 'test5'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME.getVariableName() + " != 'test5'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_NAME.getVariableName() + " != 'test5'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallEeMethodName

# 13. ee_method_arg_num - 方法调用判断被调用方法参数数量

- 表达式变量说明

被调用方方法参数数量

- 表达式示例说明

在解析方法调用时，判断被调用方法参数数量是否等于指定值，仅处理匹配的方法调用

- 表达式示例文本

```js
ee_method_arg_num != 5
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM.getVariableName() + " != 5"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_METHOD_ARG_NUM.getVariableName() + " != 5"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallEeMethodArgNum

# 14. ee_full_method - 方法调用判断被调用完整方法

- 表达式变量说明

被调用方完整方法

包括括号及方法参数

- 表达式示例说明

在解析方法调用时，判断被调用完整方法是否等于指定关键字，仅处理匹配的方法调用

- 表达式示例文本

```js
ee_full_method != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'
```

- 表达式示例文本 - 代码中指定

```java
CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_PARSE_IGNORE_METHOD_CALL, 
    CommonElAllowedVariableEnum.EAVE_MC_EE_FULL_METHOD.getVariableName() + " != 'test.callgraph.annotation.CallMethodWithAnnotation:test1()'"
);
```

- 表达式示例类名

test.el.methodcall.TestElParseIgnoreMethodCallEeFullMethod

