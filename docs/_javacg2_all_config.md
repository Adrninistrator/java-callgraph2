# 1. 说明

每个配置参数可以通过配置文件或对应的枚举进行修改，效果相同

当前文件代表所有支持的的配置参数

# 2. 主要的配置文件参数

## 2.1. _javacg2_config/config.properties

- 配置文件枚举类名

JavaCG2ConfigKeyEnum

### 2.1.1. parse.method.call.type.value

- 参数说明

```
处理方法调用时是否解析被调用对象和参数可能的类型与值
开启后可支持识别多态、Spring Bean等使用的实际类型
例如对于方法调用 A a = new B(); a.func(123); 开启当前开关后可获得对象a类型为B，func方法调用时参数值为123
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_PARSE_METHOD_CALL_TYPE_VALUE|

### 2.1.2. first.parse.init.method.type

- 参数说明

```
处理类的方法前是否需要先解析构造函数以获取非静态字段可能的类型，仅当parse.method.call.type.value参数为true时才可以生效
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_FIRST_PARSE_INIT_METHOD_TYPE|

### 2.1.3. analyse.field.relationship

- 参数说明

```
是否需要分析dto的字段之间通过get/set方法的关联关系，仅当parse.method.call.type.value参数为true时才可以生效
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|false|
|参数枚举名|CKE_ANALYSE_FIELD_RELATIONSHIP|

### 2.1.4. continue.when.error

- 参数说明

```
解析方法出现异常时，是否要继续
若开启后在出现异常时不会抛出异常，会继续执行；若不开启则出现异常时会抛出异常终止处理
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_CONTINUE_WHEN_ERROR|

### 2.1.5. log.method.spend.time

- 参数说明

```
是否在输出目录生成记录方法分析耗时的文件
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|true|
|参数默认值|true|
|参数枚举名|CKE_LOG_METHOD_SPEND_TIME|

### 2.1.6. output.root.path

- 参数说明

```
生成文件的根目录，分隔符支持使用/或\，末尾是否为分隔符不影响
默认使用指定的需要解析的jar文件所在目录
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值||
|参数默认值||
|参数枚举名|CKE_OUTPUT_ROOT_PATH|

### 2.1.7. output.file.ext

- 参数说明

```
指定生成文件后缀名，需要以“.”开头
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值|.md|
|参数默认值|.txt|
|参数枚举名|CKE_OUTPUT_FILE_EXT|

### 2.1.8. el.debug.mode

- 参数说明

```
是否开启表达式执行调试模式，若开启会在应用日志中输出表达式执行时的详细信息
```

|描述|内容|
|---|---|
|参数类型|Boolean|
|参数值是否必填|否|
|当前使用参数值|false|
|参数默认值|false|
|参数枚举名|CKE_EL_DEBUG_MODE|

### 2.1.9. handle.callee.new.raw.actual

- 参数说明

```
解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型的开关
only_raw: 记录一条方法调用关系，被调用类型使用：原始类型	only_actual: 记录一条方法调用关系，被调用类型使用：实际类型	raw_actual: 记录两条方法调用关系，被调用类型分别使用：原始类型、实际类型
例如 Super1 obj = new Child1(); obj.func1(); ，被调用对象的原始类型为Super1，实际类型为Child1，通过该开关选择被调用对象使用的类型
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值|only_actual|
|参数默认值|only_actual|
|参数枚举名|CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL|

### 2.1.10. handle.callee.spring.bean.raw.actual

- 参数说明

```
解析方法调用时，被调用对象为Spring Bean时（支持字段注入），类型使用原始类型还是实际类型的开关
only_raw: 记录一条方法调用关系，被调用类型使用：原始类型	only_actual: 记录一条方法调用关系，被调用类型使用：实际类型	raw_actual: 记录两条方法调用关系，被调用类型分别使用：原始类型、实际类型
例如Spring Bean字段定义的类型为Super1，实际注入的类型为Child1，被调用对象的原始类型为Super1，实际类型为Child1，通过该开关选择被调用对象使用的类型
```

|描述|内容|
|---|---|
|参数类型|String|
|参数值是否必填|否|
|当前使用参数值|only_actual|
|参数默认值|only_actual|
|参数枚举名|CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL|


# 3. 不区分顺序的其他配置信息

## 3.1. _javacg2_config/fr_eq_conversion_method.properties

- 配置文件枚举类名与枚举名

JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD

- 参数说明

(作用) 在分析dto的字段之间通过get/set方法的关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法（每行代表一条记录，支持多行）

(内容) key指定对应的方法，包含{完整类名}:{方法名}

(内容) value指定与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号

(格式) {完整类名}:{方法名}={被调用对象或方法参数序号}

- 当前使用参数值

```
java.lang.Boolean:<init>=1
java.lang.Boolean:parseBoolean=1
java.lang.Boolean:valueOf=1
java.lang.Double:<init>=1
java.lang.Double:parseDouble=1
java.lang.Double:valueOf=1
java.lang.Float:<init>=1
java.lang.Float:parseFloat=1
java.lang.Float:valueOf=1
java.lang.Integer:<init>=1
java.lang.Integer:parseInt=1
java.lang.Integer:valueOf=1
java.lang.Long:<init>=1
java.lang.Long:parseLong=1
java.lang.Long:valueOf=1
java.lang.String:<init>=1
java.lang.String:trim=0
java.lang.String:valueOf=1
java.math.BigDecimal:<init>=1
java.math.BigDecimal:=0
java.math.BigDecimal:toString=0
java.math.BigDecimal:valueOf=1
org.apache.commons.lang.StringUtils:defaultIfBlank=1
org.apache.commons.lang.StringUtils:defaultIfEmpty=1
org.apache.commons.lang.StringUtils:defaultString=1
org.apache.commons.lang.StringUtils:trim=1
org.apache.commons.lang.math.NumberUtils:createBigDecimal=1
org.apache.commons.lang.math.NumberUtils:createBigInteger=1
org.apache.commons.lang.math.NumberUtils:createDouble=1
org.apache.commons.lang.math.NumberUtils:createFloat=1
org.apache.commons.lang.math.NumberUtils:createInteger=1
org.apache.commons.lang.math.NumberUtils:createLong=1
org.apache.commons.lang.math.NumberUtils:createNumber=1
org.apache.commons.lang3.StringUtils:defaultIfBlank=1
org.apache.commons.lang3.StringUtils:defaultIfEmpty=1
org.apache.commons.lang3.StringUtils:defaultString=1
org.apache.commons.lang3.StringUtils:trim=1
org.apache.commons.lang3.math.NumberUtils:createBigDecimal=1
org.apache.commons.lang3.math.NumberUtils:createBigInteger=1
org.apache.commons.lang3.math.NumberUtils:createDouble=1
org.apache.commons.lang3.math.NumberUtils:createFloat=1
org.apache.commons.lang3.math.NumberUtils:createInteger=1
org.apache.commons.lang3.math.NumberUtils:createLong=1
org.apache.commons.lang3.math.NumberUtils:createNumber=1
```

# 4. 区分顺序的其他配置信息

## 4.1. _javacg2_config/jar_dir.properties

- 配置文件枚举类名与枚举名

JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR

- 参数说明

(作用) 指定需要解析的jar、war文件路径，或保存class、jar、war文件的目录路径（每行代表一条记录，支持多行）

(格式) 路径中的分隔符支持使用/或\，目录最后指定或不指定分隔符均可

(示例) build/

(示例) build/test.jar

(示例) D:/test/build/test.jar

- 当前使用参数值

```
build/test.jar
```

