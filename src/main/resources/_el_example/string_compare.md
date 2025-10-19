# 1. 表达式配置文件说明

当前文件的内容为表达式中的字符串比较示例

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

# 2. class_name - 判断多个条件使用与

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，多个条件使用与运算，判断类名是否以指定关键字开头，且以指定关键字结尾，忽略匹配的类

- 表达式示例文本

```js
string.startsWith(class_name, 'test.callgraph.annotation.') && string.endsWith(class_name, 'CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.') && string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameAnd

# 3. class_name - 判断包含关键字

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否包含指定关键字，忽略匹配的类

- 表达式示例文本

```js
string.contains(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.contains(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameContains

# 4. class_name - 判断包含多个关键字之一

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否包含指定多个关键字之一，忽略匹配的类

- 表达式示例文本

```js
string.containsAny(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.containsAny(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameContainsAny

# 5. class_name - 判断包含关键字（忽略大小写）

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否包含指定关键字（忽略大小写），忽略匹配的类

- 表达式示例文本

```js
string.containsIC(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.containsIC(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameContainsIC

# 6. class_name - 判断以关键字结尾

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否以指定关键字结尾，忽略匹配的类

- 表达式示例文本

```js
string.endsWith(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameEndsWith

# 7. class_name - 判断以多个关键字之一结尾

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否以指定多个关键字之一结尾，忽略匹配的类

- 表达式示例文本

```js
string.endsWithAny(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWithAny(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameEndsWithAny

# 8. class_name - 判断以关键字结尾（忽略大小写）

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否以指定关键字结尾（忽略大小写），忽略匹配的类

- 表达式示例文本

```js
string.endsWithIC(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.endsWithIC(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameEndsWithIC

# 9. class_name - 判断等于关键字

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

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameEquals

# 10. class_name - 判断等于多个关键字之一

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否等于指定多个关键字之一，忽略匹配的类

- 表达式示例文本

```js
string.equalsAny(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.equalsAny(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameEqualsAny

# 11. class_name - 判断等于关键字（忽略大小写）

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否等于指定关键字（忽略大小写），忽略匹配的类

- 表达式示例文本

```js
string.equalsIC(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.equalsIC(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameEqualsIC

# 12. class_name - 判断多个条件使用或

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，多个条件使用或运算，判断类名是否等于指定多个关键字中的任意一个，忽略匹配的类

- 表达式示例文本

```js
class_name == 'test.callgraph.annotation.CallMethodWithAnnotation' || class_name == 'test.callgraph.annotation.MethodWithAnnotation'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == 'test.callgraph.annotation.CallMethodWithAnnotation' || " + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + " == 'test.callgraph.annotation.MethodWithAnnotation'"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameOr

# 13. class_name - 判断以关键字开头

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否以指定关键字开头，忽略匹配的类

- 表达式示例文本

```js
string.startsWith(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWith(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameStartsWith

# 14. class_name - 判断以多个关键字之一开头

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否以指定多个关键字之一开头，忽略匹配的类

- 表达式示例文本

```js
string.startsWithAny(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWithAny(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation', 'test.callgraph.annotation.MethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameStartsWithAny

# 15. class_name - 判断以关键字开头（忽略大小写）

- 表达式变量说明

完整类名

- 表达式示例说明

在解析类时，判断类名是否以指定关键字开头（忽略大小写），忽略匹配的类

- 表达式示例文本

```js
string.startsWithIC(class_name, 'test.callgraph.annotation.CallMethodWithAnnotation')
```

- 表达式示例文本 - 代码中指定

```java
"string.startsWithIC(" + JavaCG2ElAllowedVariableEnum.EAVE_PARSE_CLASS_NAME.getVariableName() + ", 'test.callgraph.annotation.CallMethodWithAnnotation')"
```

- 表达式示例类名

test.el.stringcompare.TestElParseIgnoreClassClassNameStartsWithIC

