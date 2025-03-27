# 1. 说明

当前文件中的配置文件只有基本的说明，各配置文件的详细说明请打开对应的配置文件查看

每个配置参数可以通过配置文件或对应的枚举进行修改，效果相同

# 2. 主要的配置信息

## 2.1. _javacg2_config/config.properties

- 配置文件枚举类名

JavaCG2ConfigKeyEnum

|参数名称|参数枚举名|参数说明|参数值|
|---|---|---|---|
|parse.method.call.type.value|CKE_PARSE_METHOD_CALL_TYPE_VALUE|处理方法调用时是否解析被调用对象和参数可能的类型与值|true|
|first.parse.init.method.type|CKE_FIRST_PARSE_INIT_METHOD_TYPE|处理类的方法前是否需要先解析构造函数以获取非静态字段可能的类型，仅当parse.method.call.type.value参数为true时才可以生效|true|
|analyse.field.relationship|CKE_ANALYSE_FIELD_RELATIONSHIP|是否需要分析dto的字段之间的关联关系，仅当parse.method.call.type.value参数为true时才可以生效|true|
|continue.when.error|CKE_CONTINUE_WHEN_ERROR|解析方法出现异常时，是否要继续。true: 继续；false: 不继续|false|
|log.method.spend.time|CKE_LOG_METHOD_SPEND_TIME|记录方法分析耗时的开关（开启后会在输出目录中生成相关文件）。true: 开启；false: 关闭|true|
|output.root.path|CKE_OUTPUT_ROOT_PATH|生成文件的根目录，以"/"或"\\"作为分隔符，末尾是否为分隔符不影响（默认为jar包所在目录）||
|output.file.ext|CKE_OUTPUT_FILE_EXT|生成文件后缀名|.md|
|el.debug.mode|CKE_EL_DEBUG_MODE|表达式执行时是否开启调试模式，若开启会在应用日志中输出表达式执行时的详细信息|false|
|handle.callee.new.raw.actual|CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL|解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型 例如 Super1 obj = new Child1(); obj.func1(); ，则被调用对象的原始类型为Super1，实际类型为Child1 only_raw 仅记录原始类型	only_actual 仅记录实际类型	raw_actual 记录原始类型+实际类型|only_actual|
|handle.callee.spring.bean.raw.actual|CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL|解析方法调用时，被调用对象为Spring Bean，类型使用原始类型还是实际类型（支持字段注入、getBean()方法） 例如Spring Bean字段定义的类型为Super1，实际注入的类型为Child1，则被调用对象的原始类型为Super1，实际类型为Child1 only_raw 仅记录原始类型	only_actual 仅记录实际类型	raw_actual 记录原始类型+实际类型|only_actual|

# 3. 不区分顺序的其他配置信息

## 3.1. _javacg2_config/fr_eq_conversion_method.properties

- 配置文件枚举类名与枚举名

JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD

- 参数说明

(作用) 在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数认为是等值转换的方法（每行代表一条记录，支持多行）

(内容) key指定对应的方法，包含{完整类名}:{方法名}

(内容) value指定与方法返回值等值的被调用对象（使用0表示）或方法参数（从1开始）序号

(格式) {完整类名}:{方法名}={被调用对象或方法参数序号}

- 参数值

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

(作用) 指定需要处理的jar、war包路径，或保存class、jar、war文件的目录路径（每行代表一条记录，支持多行）

(格式) 路径中的分隔符使用/或\均可，目录最后指定或不指定分隔符均可

(示例) build/libs/

(示例) build/libs/test.jar

(示例) D:/test/build/libs/test.jar

- 参数值

```
java-all-call-graph/build/libs/test.jar
```

