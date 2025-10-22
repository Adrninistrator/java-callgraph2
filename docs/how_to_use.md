# 1. 使用说明

## 1.1. 依赖环境

需要使用 JDK8 及以上版本

若需要通过 IDE 打开项目源码运行，建议安装 Gradle 管理依赖库

## 1.2. 配置参数

### 1.2.1. 支持的参数配置方式

```
通过配置文件指定
通过代码指定
```

以上两种方式的效果是相同的，每个配置文件及配置参数在代码中都存在对应项

### 1.2.2. 支持的配置参数格式

支持以下三种格式的配置参数

#### 1.2.2.1. Map 格式-key value 形式的参数

当前项目中各个参数的对应枚举为 com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum

每个枚举常量代表一个参数，对应的配置文件都是 _javacg2_config/config.properties

参数为键值对形式，每个参数指定唯一的值

#### 1.2.2.2. List 格式-区分顺序的参数

当前项目中的对应枚举为 com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值区分顺序，可指定多个值

#### 1.2.2.3. Set 格式-不区分顺序的参数

当前项目中的对应枚举为 com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值不区分顺序，可指定多个值

#### 1.2.2.4. EL 表达式

当前项目中的对应枚举为 com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值需要指定 EL 表达式，整个参数值是一个表达式

### 1.2.3. 配置参数示例

参考 [配置参数示例](docs/_javacg2_all_config.md)

### 1.2.4. 主要的配置文件

_javacg2_config/jar_dir.properties

指定需要解析的 jar、war、jmod 文件路径，或保存 class、jar、war、jmod 文件的目录路径

### 1.2.5. 表达式使用通用说明文档

参考 [表达式使用通用说明文档](src/main/resources/_el_example/el_usage.md)

### 1.2.6. 表达式字符串比较说明文档

参考 [表达式字符串比较说明文档](src/main/resources/_el_example/string_compare.md)

### 1.2.7. 通过代码指定配置参数

在代码中使用 com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper 类可以指定配置参数

在创建 com.adrninistrator.javacg2.entry.JavaCG2Entry 类实例时需要有参数的构造函数“JavaCG2Entry(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper)”

以下为 JavaCG2ConfigureWrapper 用于指定配置参数的方法

|方法名称|方法作用|
|---|---|
|setMainConfig|设置 key value 形式的参数|
|setOtherConfigList|设置区分顺序的参数|
|setOtherConfigSet|设置不区分顺序的参数|
|setElConfigText|设置 EL 表达式|

## 1.3. 运行方式

### 1.3.1. 项目入口类

执行以下类可对指定的 jar 文件等进行静态分析

```
com.adrninistrator.javacg2.entry.JavaCG2Entry
```

### 1.3.2. 支持的运行方式

```
通过 IDE 打开项目源码运行
在其他项目中引用当前项目的库运行
使用项目源码构建后通过 Java 命令运行
```

### 1.3.3. 各种运行方式支持的参数配置方式

|运行方式|支持的参数配置方式|
|---|---|
|通过 IDE 打开项目源码运行|通过配置文件指定<br>通过代码指定|
|在其他项目中引用当前项目的库运行|通过配置文件指定<br>通过代码指定|
|使用项目源码构建后通过 Java 命令运行|通过配置文件指定|

### 1.3.4. 通过 IDE 打开项目源码运行

通过 IDE 打开当前项目，由 Gradle 管理依赖库，可使用源码运行

### 1.3.5. 在其他项目中引用当前项目的库运行

在其他的项目中，使用 Maven/Gradle 等管理依赖库，并添加对当前项目的依赖

- 使用 Maven 管理依赖

```xml
<dependency>
    <groupId>com.github.adrninistrator</groupId>
    <artifactId>java-callgraph2</artifactId>
    <version>版本号</version>
    <type>pom</type>
</dependency>
```

- 使用 Gradle 管理依赖

```
implementation("com.github.adrninistrator:java-callgraph2: 版本号")
```

### 1.3.6. 使用项目源码构建后通过 Java 命令运行

#### 1.3.6.1. 构建方式

在项目根目录执行以下命令

```
gradlew gen_run_jar
```

#### 1.3.6.2. 生成的文件

构建完成后，会在项目根目录 jar_output_dir 生成相关目录及文件

|目录、文件名|作用|
|---|---|
|_javacg2_xxx|当前项目使用的配置文件保存目录|
|config|log4j2 配置文件保存目录|
|jar|当前项目编译生成的 jar 文件保存目录|
|lib|当前项目的依赖库 jar 文件|
|run.bat|用于执行当前项目解析 Java 代码的脚本|
|run.sh|用于执行当前项目解析 Java 代码的脚本|

#### 1.3.6.3. 执行方式

对配置文件进行配置后，可执行 run.bat 或 run.sh 脚本，解析指定的 Java 代码

## 1.4. 示例代码

### 1.4.1. 生成用于解析的示例 jar 文件

在项目根目录执行以下命令，可生成用于解析的示例 jar 文件 build/test.jar

```shell
gradlew test_gen_jar
```

### 1.4.2. 可直接执行的示例方法

参考示例方法 test.parse.TestParse:testParseJavaCG2TestLib，会对以上生成的示例 jar 文件进行解析

示例方法代码如下：

```java
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/test.jar");
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
```
