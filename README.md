[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. 项目说明

java-callgraph2 项目用于对 Java 代码（编译后的 class、jar、war、jmod 等文件）进行静态分析，支持输出的文件见 [生成文件说明](docs/file_desc.md)

当前项目原本 fork 自 [https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)，用于生成 Java 方法调用关系

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供通过静态分析获取 Java 方法调用关系的功能，因此创建了该项目

当前项目只会输出静态分析结果到文件，不会写入数据库；假如需要将结果写入数据库进行后续分析，例如生成 Java 代码完整方法调用链、生成调用堆栈、JarDiff 分析 jar 文件方法修改影响范围等，可使用项目 [https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

`当前项目提供了插件功能，可用于为 Java 代码自动生成 UML 时序图（文档未完成，暂未提交）`，可参考 [https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)。

# 2. 让大模型基于项目回答问题

## 2.1. DeepWiki

提问不需要注册，但不再检索项目的最新内容

[https://deepwiki.com/Adrninistrator/java-callgraph2](https://deepwiki.com/Adrninistrator/java-callgraph2)

通过大模型分析项目代码，可向大模型提出关于项目的问题，包括使用方法等

## 2.2. zread.ai

提问需要注册

[https://zread.ai/Adrninistrator/java-callgraph2](https://zread.ai/Adrninistrator/java-callgraph2)

作用同上

# 3. 特性说明

原始 java-callgraph 在多数场景下能够获取到 Java 方法调用关系，但以下场景的调用关系会缺失

- 接口与实现类方法

假如存在接口 Interface1，及其实现类 Impl1，若在某个类 Class1 中引入了接口 Interface1，实际为实现类 Impl1 的实例（使用 Spring 时的常见场景），在其方法 Class1.func1() 中调用了 Interface1.fi() 方法；

原始 java-callgraph 生成的方法调用关系中，只包含 Class1.func1() 调用 Interface1.fi() 的关系，Class1.func1() 调用 Impl1.fi()，及 Impl1.fi() 向下调用的关系会缺失。

- Runnable 实现类线程调用

假如 f1() 方法中使用内部匿名类形式的 Runnable 实现类在线程中执行操作，在线程中执行了 f2() 方法，如下所示

```java
private void f1() {
    new Thread(new Runnable() {
        @Override
        public void run() {
            f2();
        }
    }).start();
}
```

原始 java-callgraph 生成的方法调用关系中，f1() 调用 f2()，及 f2() 向下调用的关系会缺失；

对于使用命名类形式的 Runnable 实现类在线程中执行操作的情况，存在相同的问题，原方法调用线程中执行的方法，及继续向下的调用关系会缺失。

- Callable 实现类线程调用

与 Runnable 实现类线程调用情况类似，略。

- Thread 子类线程调用

与 Runnable 实现类线程调用情况类似，略。

- lambda 表达式（含线程调用等）

假如 f1() 方法中使用 lambda 表达式的形式在线程中执行操作，在线程中执行了 f2() 方法，如下所示

```java
private void f1() {
    new Thread(() -> f2()).start();
}
```

原始 java-callgraph 生成的方法调用关系中，f1() 调用 f2()，及 f2() 向下调用的关系会缺失；

对于其他使用 lambda 表达式的情况，存在相同的问题，原方法调用 lambda 表达式中执行的方法，及继续向下的调用关系会缺失。

- Stream 调用

在使用 Stream 时，通过 xxx::func 方式调用方法，原始 java-callgraph 生成的方法调用关系中会缺失。如以下示例中，当前方法调用当前类的 map2()、filter2()，及 TestDto1 类的 getStr() 方法的调用关系会缺失。

```java
list.stream().map(this::map2).filter(this::filter2).collect(Collectors.toList());
list.stream().map(TestDto1::getStr).collect(Collectors.toList());
```

- 父类调用子类的实现方法

假如存在抽象父类 Abstract1，及其非抽象子类 ChildImpl1，若在某个类 Class1 中引入了抽象父类 Abstract1，实际为子类 ChildImpl1 的实例（使用 Spring 时的常见场景），在其方法 Class1.func1() 中调用了 Abstract1.fa() 方法；

原始 java-callgraph 生成的方法调用关系中，只包含 Class1.func1() 调用 Abstract1.fa() 的关系，Class1.func1() 调用 ChildImpl1.fa() 的关系会缺失。

- 子类调用父类的实现方法

假如存在抽象父类 Abstract1，及其非抽象子类 ChildImpl1，若在 ChildImpl1.fc1() 方法中调用了父类 Abstract1 实现的方法 fi()；

原始 java-callgraph 生成的方法调用关系中，ChildImpl1.fc1() 调用 Abstract1.fi() 的关系会缺失。

针对以上问题，java-callgraph2 都进行了优化，能够生成缺失的调用关系。

对于更复杂的情况，例如存在接口 Interface1，及其抽象实现类 Abstract1，及其子类 ChildImpl1，若在某个类中引入了抽象实现类 Abstract1 并调用其方法的情况，生成的方法调用关系中也不会出现缺失。

# 4. 主要步骤

当前项目在对 Java 代码进行静态分析时，主要的步骤如下

```
处理需要解析的文件
解析合并后的或原始 jar 文件
遍历并解析 jar 文件中的类
解析结果输出到文件
```

# 5. 对需要解析文件的处理

## 5.1. 支持解析的文件格式

支持对以下文件进行解析

|文件类型|说明|
|---|---|
|class|在解析时需要指定 class 文件所在的目录|
|jar|支持解析 jar 文件中的 class 等文件，及 jar 文件中的 jar 文件（Spring Boot 编译生成的 jar）中的文件|
|war|支持解析 war 文件中的 class 等文件，及 war 文件中的 jar 文件中的文件|
|jmod|JDK9 及以上的 JDK 标准库文件格式|
|xml|支持解析 Spring、MyBatis 相关的 XML 数据（需要通过 java-all-call-graph 实现）|
|properties|解析 properties 文件内容|

## 5.2. 直接使用原始 jar 文件

假如指定需要解析的文件是指定了一个 jar 文件，且该 jar 文件中不再存在 jar 文件，则会直接使用原始 jar 文件进行解析

## 5.3. 对指定需要解析的文件、目录进行合并

- 需要合并的情况

在以下情况下，会将指定需要解析的文件、目录（中的文件）合并为一个 jar 文件

```
指定的需要解析的文件多于一个
指定的需要解析的内容包含目录
指定的需要解析的是 war 文件且其中包含 jar 文件
指定的需要解析的是 jar 文件且其中包含 jar 文件
指定的需要解析的是 jmod 文件
```

- 合并生成的 jar 文件保存目录

若指定的需要解析的第一个元素为 jar、war、jmod 文件，则新生成的 jar 文件生成在同一个目录中

若指定的需要解析的第一个元素为目录，则新生成的 jar 文件生成在该目录中

文件名在第一个元素名称之后增加“-javacg2_merged.jar”作为后缀

- 合并生成的 jar 文件结构

合并生成的 jar 文件的第一层目录名为格式为“{序号}@{原 jar、war、jmod 文件或目录名}”，如“0001@test.jar”

### 5.3.1. multi-release JAR 文件处理

假如需要解析的 jar 文件包含 multi-release JAR，则需要在_javacg2_config/config.properties 配置文件中指定参数 jdk.runtime.major.version，在合并 multi-release JAR 时使用指定版本的 class 文件，即“META-INF/versions/{JDK 主版本号}”目录中的 class 文件

# 6. 支持输出的文件格式

具体文件格式见 [输出文件格式](docs/file_format.md) ，有部分文件需要使用 java-all-call-graph 组件时支持输出

## 6.1. 方法调用类型

生成的方法调用关系文件中的该当调用类型可参考 [方法调用类型](docs/call_type.md)

# 7. 使用说明

## 7.1. 依赖环境

需要使用 JDK8 及以上版本

若需要通过 IDE 打开项目源码运行，建议安装 Gradle 管理依赖库

## 7.2. 配置参数

### 7.2.1. 支持的参数配置方式

```
通过配置文件指定
通过代码指定
```

以上两种方式的效果是相同的，每个配置文件及配置参数在代码中都存在对应项

### 7.2.2. 支持的配置参数格式

支持以下三种格式的配置参数

#### 7.2.2.1. Map 格式-key value 形式的参数

当前项目中各个参数的对应枚举为 com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum

每个枚举常量代表一个参数，对应的配置文件都是 _javacg2_config/config.properties

参数为键值对形式，每个参数指定唯一的值

#### 7.2.2.2. List 格式-区分顺序的参数

当前项目中的对应枚举为 com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值区分顺序，可指定多个值

#### 7.2.2.3. Set 格式-不区分顺序的参数

当前项目中的对应枚举为 com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseSetEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值不区分顺序，可指定多个值

#### 7.2.2.4. EL 表达式

当前项目中的对应枚举为 com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum

每个枚举常量代表一个参数，对应一个配置文件

参数值需要指定 EL 表达式，整个参数值是一个表达式

### 7.2.3. 配置参数示例

参考 [配置参数示例](docs/_javacg2_all_config.md)

### 7.2.4. 重要配置文件

_javacg2_config/jar_dir.properties

指定需要解析的 jar、war、jmod 文件路径，或保存 class、jar、war、jmod 文件的目录路径

### 7.2.5. 表达式使用通用说明文档

参考 [表达式使用通用说明文档](src/main/resources/_el_example/el_usage.md)

### 7.2.6. 表达式字符串比较说明文档

参考 [表达式字符串比较说明文档](src/main/resources/_el_example/string_compare.md)

### 7.2.7. 通过代码指定配置参数

在代码中使用 com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper 类可以指定配置参数

在创建 com.adrninistrator.javacg2.entry.JavaCG2Entry 类实例时需要有参数的构造函数“JavaCG2Entry(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper)”

以下为 JavaCG2ConfigureWrapper 用于指定配置参数的方法

|方法名称|方法作用|
|---|---|
|setMainConfig|设置 key value 形式的参数|
|setOtherConfigList|设置区分顺序的参数|
|setOtherConfigSet|设置不区分顺序的参数|
|setElConfigText|设置 EL 表达式|

## 7.3. 运行方式

### 7.3.1. 项目入口类

执行以下类可对指定的 jar 文件等进行静态分析

```
com.adrninistrator.javacg2.entry.JavaCG2Entry
```

### 7.3.2. 支持的运行方式

```
通过 IDE 打开项目源码运行
在其他项目中引用当前项目的库运行
使用项目源码构建后通过 Java 命令运行
```

### 7.3.3. 各种运行方式支持的参数配置方式

|运行方式|支持的参数配置方式|
|---|---|
|通过 IDE 打开项目源码运行|通过配置文件指定<br>通过代码指定|
|在其他项目中引用当前项目的库运行|通过配置文件指定<br>通过代码指定|
|使用项目源码构建后通过 Java 命令运行|通过配置文件指定|

### 7.3.4. 通过 IDE 打开项目源码运行

通过 IDE 打开当前项目，由 Gradle 管理依赖库，可使用源码运行

#### 7.3.4.1. 通过配置文件指定参数运行

假如需要通过配置文件指定参数，可修改项目中的配置文件相关，再运行项目入口类

项目运行模块需要选择 test，以使 test 模块中的 log4j2 配置文件生效

#### 7.3.4.2. 通过代码指定参数运行

假如需要通过代码指定参数，可直接执行示例方法 test.parse.TestParse:testParseJavaCG2TestLib 进行验证，或修改参数后解析指定的 jar 文件

### 7.3.5. 在其他项目中引用当前项目的库运行

#### 7.3.5.1. 依赖库管理

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

#### 7.3.5.2. 通过配置文件指定参数运行

假如需要通过配置文件指定参数，则需要将 java-callgraph2 项目的 src/main/resources 目录中以`_javacg2_`开头的目录复制到其他项目的 src/main/resources 或 src/test/resources 目录

修改配置文件相关参数后，可运行入口类

#### 7.3.5.3. 通过代码指定参数运行

同上

### 7.3.6. 使用项目源码构建后通过 Java 命令运行

#### 7.3.6.1. 构建方式

在项目根目录执行以下命令

```
gradlew gen_run_jar
```

#### 7.3.6.2. 生成的文件

构建完成后，会在项目根目录 jar_output_dir 生成相关目录及文件

|目录、文件名|作用|
|---|---|
|_el_example|表达式相关的示例与说明文件|
|_javacg2_xxx|当前项目使用的配置文件保存目录|
|config|log4j2 配置文件保存目录|
|jar|当前项目编译生成的 jar 文件保存目录|
|lib|当前项目的依赖库 jar 文件|
|run.bat|用于执行当前项目解析 Java 代码的脚本|
|run.sh|用于执行当前项目解析 Java 代码的脚本|

#### 7.3.6.3. 通过配置文件指定参数运行

对配置文件参数进行配置后，可执行 run.bat 或 run.sh 脚本，解析指定的 Java 代码

## 7.4. 示例代码

### 7.4.1. 生成用于解析的示例 jar 文件

在项目根目录执行以下命令，可生成用于解析的示例 jar 文件 build/test.jar

```shell
gradlew test_gen_jar
```

### 7.4.2. 可直接执行的示例方法

参考示例方法 test.parse.TestParse:testParseJavaCG2TestLib，会对以上生成的示例 jar 文件进行解析

示例方法代码如下：

```java
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR, "build/test.jar");
        JavaCG2Entry javaCG2Entry = new JavaCG2Entry(javaCG2ConfigureWrapper);
        Assert.assertTrue(javaCG2Entry.run());
```

# 8. 更新说明

[更新说明](docs/change_log.md)
