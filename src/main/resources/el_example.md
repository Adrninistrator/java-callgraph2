# 表达式配置文件说明
当前文件为表达式示例配置文件，使用 aviator 表达式组件，语法与 Java 类似
使用文档可参考 https://www.yuque.com/boyan-avfmj/aviatorscript
每个配置文件的表达式的执行结果类型需要为 boolean ，即结果要么是 true ，要么是 false
通过表达式的执行结果，决定配置文件所对应场景下执行什么操作
配置文件中有说明允许使用的变量信息

# 表达式示例
例如在 _javacg2_parse_class_method_switch/parse_ignore_class.av 配置文件中指定以下内容
package_name == 'a.b' && string.endsWith(class_name, 'Test')
代表在解析类时，假如类的包名等于 'a.b'，且类的简单类名以 'Test' 结尾，则跳过解析对应的类

# 表达式语法 - aviator 默认支持

## 返回固定值

### true
若表达式配置为“true”，则表达式执行结果固定为 true

### false
若表达式配置为“false”，或未指定表达式，则表达式执行结果固定为 false

## 字符串处理
除判断字符串是否等于指定值外，需要使用 aviator 提供的 string.xxx() 函数对字符串进行判断
字符串常量可以使用单引号包含，如 'abc'

### ==
（作用）判断字符串类型的变量是否等于指定内容
（语法）{字符串类型变量名称} == {常量字符串}
（示例）str1 == 'abc'

### string.startsWith()
（作用）判断字符串类型的变量是否以指定内容开头
（语法）string.startsWith({字符串类型变量名称}, {常量字符串})
（示例）string.startsWith(str1, 'abc')

### string.endsWith()
（作用）判断字符串类型的变量是否以指定内容结尾
（语法）string.endsWith({字符串类型变量名称}, {常量字符串})
（示例）string.endsWith(str1, 'abc')

### string.contains()
（作用）判断字符串类型的变量是否包含指定内容
（语法）string.contains({字符串类型变量名称}, {常量字符串})
（示例）string.contains(str1, 'abc')

### string.length()
（作用）获取字符串类型的变量的长度
（语法）string.length({字符串类型变量名称})
（示例）string.length(str1)

## 整型处理
整形的判断与 Java 语法相同，可使用比较运算符：==、<、>、<=、>=、!=
（语法）{整型变量名称} {比较运算符} {常量整形值}
（示例）int1 == 1
（示例）int1 != 1
（示例）int1 >= 1

## 逻辑判断
aviator 支持的逻辑判断运算符与 Java 相同

### &&
（作用）判断两个条件是否同时成立，只有两个条件都为 true 时，整体结果才为 true
（语法）{条件1} && {条件2}
（示例）x > 10 && y < 20

### ||
（作用）判断两个条件中是否有一个成立，只要有一个条件为 true，整体结果就为 true
（语法）{条件1} || {条件2}
（示例）x > 10 || y < 20

### !
（作用）取反运算符，用于将条件的结果取反，若条件为 true，则结果为 false；若条件为 false，则结果为 true
（语法）!{条件}
（示例）!(x > 10)

### ()
（作用）用于改变运算顺序，确保按照期望的顺序执行多个逻辑表达式
（语法）({表达式1} {运算符} {表达式2})
（示例）(x > 10 && y < 20) || z == 5
