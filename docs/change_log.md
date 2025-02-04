# 1. 更新说明

## 1.1. (0.1.2)

- 支持对目录进行处理

支持对目录中的class、jar/war文件进行处理

支持在启动参数中指定一个或多个jar/war包或目录

在处理时，会将指定的jar/war包及指定目录合并为一个新的jar包后再处理：

a. 对于指定的jar/war包及指定目录中的后缀为.jar/.war的文件，将其中的class文件进行合并

b. 对于指定目录中的后缀非.jar/.war的文件进行合并

合并jar/war包中的class文件时，支持仅将指定包名的class文件合并到新的jar包中

合并产生的新jar包信息如下：

保存在指定的第一个jar/war包所在目录中（若第一个是目录则在该目录中）

文件名为第一个jar/war包加上“-javacg2_merged.jar”

第一层目录为每个被合并的.jar/.war文件或目录的名称

- 支持插件功能

提供用于生成Java方法UML时序图的插件功能

- 输出文件路径指定方式变化

不再通过JVM选项“-Doutput.file=”指定输出文件路径，默认将输出文件生成在指定的第一个jar包所在目录中（若第一个是目录则在该目录中）

方法调用关系文件名为第一个jar包或合并后的jar包加上“.txt”

注解信息文件名为第一个jar包或合并后的jar包加上“-annotation.txt”

## 1.2. (0.1.3)

生成的注解信息文件中增加类上的注解信息，包含注解名称、注解属性名称及属性值；增加方法上的注解的注解属性名称及属性值

## 1.3. (0.1.5)

在对类进行处理时，跳过重复同名类

## 1.4. (0.1.7)

支持人工添加缺失的方法调用关系（定制化代码开发）

具体说明可参考[https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md)中的相关内容

## 1.5. (0.1.8)

生成方法代码行号信息文件，文件名以“-line_number.txt”结尾

## 1.6. (0.1.9)

对于某个类调用自身类的类调用关系，也会生成在输出文件中

## 1.7. (0.2.0)

在处理注解的属性值时，支持使用自定义类处理，自定义类需要实现`com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatorInterface`接口

可使用java-all-call-graph中的`com.adrninistrator.jacg.extensions.annotation_attributes.AllAnnotationAttributesFormator`类

## 1.8. (0.2.1)

对于接口之间的继承关系，在内存中进行记录

## 1.9. (0.2.2)

方法调用对象中增加字段

## 1.10. (1.0.0)

- 指定需要合并的包名时增加支持目录

通过`merge.class.in.jar.package`参数指定需要合并的包名时，增加对“BOOT-INF/classes/”目录中的class文件进行处理，对应Spring Boot Maven Plugin插件生成的jar包
参数换了

- 支持TimeTask构造函数调用run方法

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

支持分析同名的jar包、目录

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

在处理class文件时，假如class文件对应类名在当前文件中，则不处理对应的类

### 1.13.2. 增加生成的文件

```
类的继承或实现的泛型信息	class_ext_impl_generics_type
类的签名中的泛型信息	class_signature_generics_type
java-callgraph2组件使用的配置参数   javacg2_config
```

### 1.13.3. 删除生成的文件

```
类的签名中继承或实现的泛型关系	class_sig_ext_impl_generics
类的签名中涉及继承与实现的信息1	class_signature_ei1
类的签名中的泛型信息	class_signature_generics
```

### 1.13.4. 生成的文件增加字段

主要涉及内容：是否数组、泛型相关的数据、Spring Controller方法是否可能用于文件/上传下载

```
非静态字段集合中涉及的泛型类型	field_generics_type
字段信息	field_info
dto的get方法及字段	get_method
方法参数集合中涉及的泛型类型	method_arg_generics_type
方法参数	method_argument
方法的信息	method_info
方法返回集合中涉及的泛型类型	method_return_generics_type
dto的set方法及字段	set_method
static、final字段初始化方法信息	sf_field_method_call
Spring Controller信息	spring_controller
```

## 1.14. (2.1.0)

### 1.14.1. 增加输出文件

```
dup_class_info  重复类的信息
dup_method_info 重复类的方法的信息
enum_init_arg_field 枚举类构造函数参数与字段赋值关系
enum_init_assign_info   枚举类初始化赋值信息
method_return_const_value    方法返回的常量值（含null）
method_return_field_info    方法返回的字段（含枚举）
```

### 1.14.2. 修改输出文件

|文件名|操作|内容|
|---|---|---|
|class_info|增加列|类在jar包中的路径|
|method_call|增加列|被调用方，对象数组的维度，为0代表不是数组类型|
|method_call|增加列|描述信息，默认为空|
