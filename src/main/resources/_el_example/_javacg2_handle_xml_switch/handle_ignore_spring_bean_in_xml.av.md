# 1. 表达式配置文件说明

- 表达式配置文件名称

_javacg2_handle_xml_switch/handle_ignore_spring_bean_in_xml.av

- 表达式配置文件作用

在 处理XML文件时 使用的表达式

若当前配置文件中的表达式执行结果为 true，则跳过对应的XML文件中的元素

若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的XML文件中的元素

指定处理XML文件中定义的Spring Bean需要跳过哪些Bean

- 表达式配置文件对应的枚举常量名

JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML

以下为支持的表达式变量示例

# 2. bean_name - XML中的Spring Bean判断Bean名称

- 表达式变量说明

Spring Bean名称

- 表达式示例说明

在处理XML文件中的Spring Bean时，判断Bean名称是否等于指定关键字，仅处理匹配的Spring Bean

- 表达式示例文本

```js
bean_name != 'testService1'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME.getVariableName() + " != 'testService1'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML, 
    JavaCG2ElAllowedVariableEnum.EAVE_SPB_BEAN_NAME.getVariableName() + " != 'testService1'"
);
```

- 表达式示例类名

test.el.handlespbinxml.TestElHandleIgnoreSPBInXmlBeanName

# 3. class_name - XML中的Spring Bean判断类名

- 表达式变量说明

Spring Bean类名

- 表达式示例说明

在处理XML文件中的Spring Bean时，判断类名是否等于指定关键字，仅处理匹配的Spring Bean

- 表达式示例文本

```js
class_name != 'a.b.C'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME.getVariableName() + " != 'a.b.C'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML, 
    JavaCG2ElAllowedVariableEnum.EAVE_SPB_CLASS_NAME.getVariableName() + " != 'a.b.C'"
);
```

- 表达式示例类名

test.el.handlespbinxml.TestElHandleIgnoreSPBInXmlClassName

# 4. profile - XML中的Spring Bean判断profile

- 表达式变量说明

Spring Bean profile

可能为空字符串，可能包含一级或多级，使用半角逗号拼接

- 表达式示例说明

在处理XML文件中的Spring Bean时，判断profile是否等于指定关键字，仅处理匹配的Spring Bean

- 表达式示例文本

```js
profile != 'dev'
```

- 表达式示例文本 - 代码中指定

```java
JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName() + " != 'dev'"
```

- 代码中指定表达式示例

```java
javaCG2ConfigureWrapper.setElConfigText(JavaCG2ElConfigEnum.ECE_HANDLE_IGNORE_SPRING_BEAN_IN_XML, 
    JavaCG2ElAllowedVariableEnum.EAVE_SPB_PROFILE.getVariableName() + " != 'dev'"
);
```

- 表达式示例类名

test.el.handlespbinxml.TestElHandleIgnoreSPBInXmlProfile

