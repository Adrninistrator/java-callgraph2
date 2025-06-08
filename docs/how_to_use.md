# 1. 使用说明

## 1.1. 依赖环境

需要使用 JDK8 及以上版本

若需要通过源码启动，建议安装 Gradle

## 1.2. 运行与配置方式

unzipfile

## 1.3. 通过源码编译

### 1.3.1. 编译方式

执行以下命令

```
gradlew jar
```

### 1.3.2. 执行方式

执行命令可参考脚本文件`run.bat`、`run.sh`

### 1.3.3. 程序主类

```java
com.adrninistrator.javacg2.entry.JavaCG2Entry
```

## 1.4. 通过组件引用

可在其他项目中通过组件方式引用当前项目，添加的 Gradle 格式的组件依赖如下

```
com.github.adrninistrator:java-callgraph2: 版本号
```

## 1.5. 配置参数

### 1.5.1. 配置参数说明及示例

参考 [_javacg2_all_config.md](docs/_javacg2_all_config.md)

### 1.5.2. _javacg2_config 目录

|配置文件名|配置文件作用|
|---|---|
|config.properties|主要的开关及输出目录等配置|
|fr_eq_conversion_method.properties|通常不需要指定，处理 get/set 方法对应的字段关联关系时使用|
|jar_dir.properties|指定需要解析的目录、jar/war 文件路径|

### 1.5.3. _javacg2_merge_file_switch 目录

用于控制在合并 jar 文件时需要忽略特定内容的开关，包括目录中的 class、jar、war 文件，jar/war 文件中的 class、jar 文件，目录、jar/war 文件中的其他类型文件等

`在 _javacg2_config/jar_dir.properties 中指定的配置包含多个 jar/war/class 文件时，仅当某个 jar/war/class 文件会被合并到最终用于解析析的 jar 文件时，才会被解析`

每个文件用于控制以上一种场景，使用表达式语言方式配置

```
ignore_class_in_dir.av
ignore_class_in_jar_war.av
ignore_jar_in_dir.av
ignore_jar_in_jar_war.av
ignore_other_in_dir.av
ignore_other_in_jar_war.av
ignore_war_in_dir.av
```

### 1.5.4. _javacg2_parse_class_method_switch 目录

控制解析类及方法时是否需要忽略的开关

每个文件用于控制以上一种场景，使用表达式语言方式配置

```
parse_ignore_class.av
parse_ignore_method.av
```

### 1.5.5. _javacg2_parse_method_call_switch 目录

控制解析方法调用时是否需要忽略的开关，包括仅通过被调用方法判断、仅通过调用方法判断、通过调用方法与被调用方法判断

每个文件用于控制以上一种场景，使用表达式语言方式配置

```
parse_ignore_method_call_ee.av
parse_ignore_method_call_er.av
parse_ignore_method_call_er_ee.av
```

### 1.5.6. 表达式语言配置示例

以上表达式语言配置示例可参考项目中的文件 src/main/resources/el_example.md

## 1.6. 配置参数-通过代码指定

在代码中使用 com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper 类可以指定配置参数

在创建 com.adrninistrator.javacg2.entry.JavaCG2Entry 类实例时需要有参数的构造函数“JavaCG2Entry(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper)”

示例如下

```java
JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, parseMethodCallTypeValue);
javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, firstParseInitMethodType);
javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, analyseFieldRelationship);
javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, continueWhenError);
javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_LOG_METHOD_SPEND_TIME, logMethodSpendTime);
javaCG2ConfigureWrapper.setOtherConfigSet(JavaCG2OtherConfigFileUseSetEnum.OCFUSE_FR_EQ_CONVERSION_METHOD,
        "java.lang.Boolean:<init>=1"
);
javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "../java-all-call-graph/build/libs/test.jar");

JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
```

# 2. 生成可以直接执行的文件

## 2.1. 编译方式

执行以下命令

```
gradlew gen_run_jar
```

## 2.2. 执行方式

执行以上命令后，会在`jar_output_dir`目录中生成可以直接执行的文件

在 Windows/Linux 等操作系统中分别执行对应的脚本文件`run.bat`、`run.sh`
