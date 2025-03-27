# 1. 更新说明

## 1.1. (0.1.2)

- 支持对目录进行处理

支持对目录中的 class、jar/war 文件进行处理

支持在启动参数中指定一个或多个 jar/war 包或目录

在处理时，会将指定的 jar/war 包及指定目录合并为一个新的 jar 包后再处理：

a. 对于指定的 jar/war 包及指定目录中的后缀为 .jar/.war 的文件，将其中的 class 文件进行合并

b. 对于指定目录中的后缀非 .jar/.war 的文件进行合并

合并 jar/war 包中的 class 文件时，支持仅将指定包名的 class 文件合并到新的 jar 包中

合并产生的新 jar 包信息如下：

保存在指定的第一个 jar/war 包所在目录中（若第一个是目录则在该目录中）

文件名为第一个 jar/war 包加上“-javacg2_merged.jar”

第一层目录为每个被合并的 .jar/.war 文件或目录的名称

- 支持插件功能

提供用于生成 Java 方法 UML 时序图的插件功能

- 输出文件路径指定方式变化

不再通过 JVM 选项“-Doutput.file=”指定输出文件路径，默认将输出文件生成在指定的第一个 jar 包所在目录中（若第一个是目录则在该目录中）

方法调用关系文件名为第一个 jar 包或合并后的 jar 包加上“.txt”

注解信息文件名为第一个 jar 包或合并后的 jar 包加上“-annotation.txt”

## 1.2. (0.1.3)

生成的注解信息文件中增加类上的注解信息，包含注解名称、注解属性名称及属性值；增加方法上的注解的注解属性名称及属性值

## 1.3. (0.1.5)

在对类进行处理时，跳过重复同名类

## 1.4. (0.1.7)

支持人工添加缺失的方法调用关系（定制化代码开发）

具体说明可参考 [https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md) 中的相关内容

## 1.5. (0.1.8)

生成方法代码行号信息文件，文件名以“-line_number.txt”结尾

## 1.6. (0.1.9)

对于某个类调用自身类的类调用关系，也会生成在输出文件中

## 1.7. (0.2.0)

在处理注解的属性值时，支持使用自定义类处理，自定义类需要实现`com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatorInterface`接口

可使用 java-all-call-graph 中的`com.adrninistrator.jacg.extensions.annotation_attributes.AllAnnotationAttributesFormator`类

## 1.8. (0.2.1)

对于接口之间的继承关系，在内存中进行记录

## 1.9. (0.2.2)

方法调用对象中增加字段

## 1.10. (1.0.0)

- 指定需要合并的包名时增加支持目录

通过`merge.class.in.jar.package`参数指定需要合并的包名时，增加对“BOOT-INF/classes/”目录中的 class 文件进行处理，对应 Spring Boot Maven Plugin 插件生成的 jar 包
参数换了

- 支持 TimeTask 构造函数调用 run 方法

- 增加配置文件及参数

```
config.properties
jar_dir.properties
packages.properties
```

- 支持通过代码指定配置参数

- 支持获取方法调用中使用的参数值、被调用对象类型等

## 1.11. (1.0.16)

在输出的`method_call`文件中，增加增加调用方法返回类型，使被调用方法返回类型更准确

## 1.12. (2.0.7)

支持分析同名的 jar 包、目录

为了使代码及配置文件与“jacg”更容易区分，对代码及配置文件进行以下名称修改

|内容|修改前的名称|修改后的名称|
|---|---|---|
|包名|com.adrninistrator.javacg|com.adrninistrator.javacg2|
|主类|com.adrninistrator.javacg.stat.JCallGraph|com.adrninistrator.javacg2.entry.JavaCG2Entry|
|目录、文件名称|javacg|javacg2|
|类名前缀|JavaCG|JavaCG2|

`当升级到该版本时，假如之前有对目录进行过分析，则相关目录中的“-javacg_merged.jar”文件需要删除`

## 1.13. (2.0.8)

### 1.13.1. 增加配置文件

_javacg2_config/ignore_class_name.properties

在处理 class 文件时，假如 class 文件对应类名在当前文件中，则不处理对应的类

### 1.13.2. 增加生成的文件

```
类的继承或实现的泛型信息	class_ext_impl_generics_type
类的签名中的泛型信息	class_signature_generics_type
java-callgraph2 组件使用的配置参数   javacg2_config
```

### 1.13.3. 删除生成的文件

```
类的签名中继承或实现的泛型关系	class_sig_ext_impl_generics
类的签名中涉及继承与实现的信息 1	class_signature_ei1
类的签名中的泛型信息	class_signature_generics
```

### 1.13.4. 生成的文件增加字段

主要涉及内容：是否数组、泛型相关的数据、Spring Controller 方法是否可能用于文件/上传下载

```
非静态字段集合中涉及的泛型类型	field_generics_type
字段信息	field_info
dto 的 get 方法及字段	get_method
方法参数集合中涉及的泛型类型	method_arg_generics_type
方法参数	method_argument
方法的信息	method_info
方法返回集合中涉及的泛型类型	method_return_generics_type
dto 的 set 方法及字段	set_method
static、final 字段初始化方法信息	sf_field_method_call
Spring Controller 信息	spring_controller
```

## 1.14. (2.1.0)

### 1.14.1. 增加输出文件

```
dup_class_info  重复类的信息
dup_method_info 重复类的方法的信息
enum_init_arg_field 枚举类构造函数参数与字段赋值关系
enum_init_assign_info   枚举类初始化赋值信息
method_return_const_value    方法返回的常量值（含 null）
method_return_field_info    方法返回的字段（含枚举）
```

### 1.14.2. 修改输出文件

|文件名|操作|内容|
|---|---|---|
|class_info|增加列|类在 jar 包中的路径|
|method_call|增加列|被调用方，对象数组的维度，为 0 代表不是数组类型|
|method_call|增加列|描述信息，默认为空|

## 1.15. (3.0.3)

### 1.15.1. 修改配置相关枚举类包名

|修改前的类名|修改后的类名|
|---|---|
|com.adrninistrator.javacg2.common.enums.JavaCG2ConfigKeyEnum|com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum|
|com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseListEnum|com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum|
|com.adrninistrator.javacg2.common.enums.JavaCG2OtherConfigFileUseSetEnum|com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum|

### 1.15.2. 修改配置相关方法名称

|修改前的方法|修改后的方法|
|---|---|
|com.adrninistrator.javacg2.conf.BaseConfigureWrapper:setConfig|com.adrninistrator.javacg2.conf.BaseConfigureWrapper:setMainConfig|

### 1.15.3. 删除配置文件

通过以下新增的配置文件能够达到相同的控制效果

```
_javacg2_config/ignore_class_name.properties
_javacg2_config/ignore_jar_file_keyword.properties
_javacg2_config/ignore_jar_file_name.properties
_javacg2_config/jar_dir_merge_file_type.properties
_javacg2_config/packages.properties
```

### 1.15.4. 增加配置文件

#### 1.15.4.1. _javacg2_merge_file_switch 目录

用于控制在合并 jar 文件时需要忽略特定内容的开关，包括目录中的 class、jar、war 文件，jar/war 文件中的 class、jar 文件，目录、jar/war 文件中的其他类型文件等

`在 _javacg2_config/jar_dir.properties 中指定的配置包含多个 jar/war/class 文件时，仅当某个 jar/war/class 文件会被合并到最终用于解析析的 jar 文件时，才会被解析`

每个文件用于控制以上一种场景，使用表达式语言方式配置：

```
ignore_class_in_dir.av
ignore_class_in_jar_war.av
ignore_jar_in_dir.av
ignore_jar_in_jar_war.av
ignore_other_in_dir.av
ignore_other_in_jar_war.av
ignore_war_in_dir.av
```

#### 1.15.4.2. _javacg2_parse_class_method_switch 目录

控制解析类及方法时是否需要忽略的开关

每个文件用于控制以上一种场景，使用表达式语言方式配置：

```
parse_ignore_class.av
parse_ignore_method.av
```

#### 1.15.4.3. _javacg2_parse_method_call_switch 目录

控制解析方法调用时是否需要忽略的开关，包括仅通过被调用方法判断、仅通过调用方法判断、通过调用方法与被调用方法判断

每个文件用于控制以上一种场景，使用表达式语言方式配置：

```
parse_ignore_method_call_ee.av
parse_ignore_method_call_er.av
parse_ignore_method_call_er_ee.av
```

### 1.15.5. 支持解析 .jar 文件中的 .jar 文件

支持解析 .jar 文件中的 .jar 文件，例如 spring-boot 等打包方式生成的 .jar 文件中 BOOT-INF/lib 目录中的 .jar 文件

## 1.16. (3.0.4)

修改对于方法调用中被调用对象或参数使用静态字段方法调用返回值时记录的数据格式

通过实现与继承关系添加的方法调用支持通过表达式配置为忽略

支持记录通过表达式忽略的数据信息到文件

用于忽略方法调用的表达式中增加方法调用类型字段

修改表达式语言使用方式示例文档

增加用于在 Aviator 表达式语言组件使用的忽略大小写字符串判断方法

## 1.17. (3.0.6)

### 1.17.1. _javacg2_config/config.properties 配置文件增加参数

```
# 表达式执行时是否开启调试模式，若开启会在应用日志中输出表达式执行时的详细信息
el.debug.mode=false
```

### 1.17.2. 表达式支持输出调试信息

见上述开关

### 1.17.3. 表达式增加允许配置的参数

```
## （允许使用的变量）{变量名称} {变量类型} {变量描述} {变量示例}
## {file_dir_path} {String} {目录中的文件所在目录绝对路径，以斜杠/为分隔符，不以分隔符结尾} {D:/a, /tmp/a}
## {file_dir_path} {String} {jar/war 文件中的文件所在目录相对路径，相对根目录的路径，以斜杠/为分隔符，不以分隔符开头或结尾} {a/b, a/b}
```

## 1.18. (3.0.7)

### 1.18.1. 增加配置参数

— _javacg2_config/config.properties

```
# 解析方法调用时，通过 new 创建的被调用类型使用原始类型还是实际类型
# 例如 Super1 obj = new Child1(); obj.func1(); ，则被调用对象的原始类型为 Super1，实际类型为 Child1
# only_raw 仅记录原始类型	only_actual 仅记录实际类型	raw_actual 记录原始类型+实际类型
handle.callee.new.raw.actual=only_actual

# 解析方法调用时，被调用对象为 Spring Bean，类型使用原始类型还是实际类型（支持字段注入、getBean() 方法）
# 例如 Spring Bean 字段定义的类型为 Super1，实际注入的类型为 Child1，则被调用对象的原始类型为 Super1，实际类型为 Child1
# only_raw 仅记录原始类型	only_actual 仅记录实际类型	raw_actual 记录原始类型+实际类型
handle.callee.spring.bean.raw.actual=only_actual
```

### 1.18.2. 增加配置文件

- _javacg2_merge_file_switch/ignore_jar_war_by_class_dir_prefix.av

```
## （作用）在 合并需要解析的目录或 jar/war 文件时 使用的表达式
## （作用）若当前配置文件中的表达式执行结果为 true，则跳过合并（及解析）对应的文件
## （作用）若表达式执行结果为 false，或未指定表达式，则当前配置不会跳过对应的文件
## （范围）通过 class 文件目录的不同层级的路径前缀判断是否跳过合并当前 jar/war 文件
## （范围）相当于通过 jar/war 文件中类的包名控制是否跳过合并当前 jar/war 文件
## （范围）以下参数为 jar/war 文件中的 class 文件目录的不同层级的路径前缀集合
## （范围）集合中的元素类型为字符串，以/作为分隔符，不会以分隔符开头或结尾
## （范围）例如 jar 文件中有 a1/c1.class、a2/b2/c2.class，则该 jar 文件中的 class 文件目录 1 级路径前缀有 a1、a2，2 级路径前缀有 a2/b2，没有大于 2 级的路径前缀
## （范围）在使用以下 class_dir_prefix_level_ 参数时，需要以 class_dir_prefix_level_ 开头，后续通过数字指定 class 文件路径层级
## （范围）例如 class_dir_prefix_level_3 代表第 3 级 class 文件路径集合
## （范围）以下的 file_name file_dir_path 均代表需要判断是否需要跳过合并的 jar/war 文件
## （表达式使用示例文件）请参考 el_example.md
## （允许使用的变量）{变量名称} {变量类型} {变量描述} {变量值示例}
## {class_dir_prefix_level_} {Set} {jar/war 文件中的 class 文件目录的不同层级的路径前缀集合} {集合：('a'), 集合：('a', 'a/b')}
## {file_name} {String} {文件名称} {a.class, a.xml}
## {file_dir_path} {String} {目录中的文件所在目录绝对路径，以斜杠/为分隔符，不以分隔符结尾} {D:/a, /tmp/a}
```

### 1.18.3. 优化 method_info 文件中的方法指令 hash

对应 java-all-call-graph 项目 method_info 数据库表的 method_instructions_hash 字段

某个类中其他方法被修改时，可能导致未被修改的方法指令 hash 发生变化，优化这个问题
