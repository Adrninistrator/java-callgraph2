[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. 说明

java-callgraph2 项目用于对 Java 代码（编译后的 class、jar、war 文件）进行静态分析

当前原本 fork 自 [https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)。

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供通过静态分析获取 Java 方法调用关系的功能，因此创建了该项目。

`当前项目提供了插件功能，可用于为 Java 代码自动生成 UML 时序图`，可参考 [https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)。

# 2. 使用说明

## 2.1. 通过源码编译

### 2.1.1. 编译方式

执行以下命令

```
gradlew jar
```

### 2.1.2. 执行方式

执行命令可参考脚本文件`run.bat`、`run.sh`

### 2.1.3. 程序主类

```java
com.adrninistrator.javacg2.entry.JavaCG2Entry
```

## 2.2. 通过组件引用

可在其他项目中通过组件方式引用当前项目，添加的 Gradle 格式的组件依赖如下：

```
com.github.adrninistrator:java-callgraph2: 版本号
```

## 2.3. 配置参数

### 2.3.1. 配置参数说明及示例

参考 [_javacg2_all_config.md](docs/_javacg2_all_config.md)

### 2.3.2. _javacg2_config 目录

|配置文件名|配置文件作用|
|---|---|
|config.properties|主要的开关及输出目录等配置|
|fr_eq_conversion_method.properties|通常不需要指定，处理 get/set 方法对应的字段关联关系时使用|
|jar_dir.properties|指定需要解析的目录、jar/war 文件路径|

### 2.3.3. _javacg2_merge_file_switch 目录

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

### 2.3.4. _javacg2_parse_class_method_switch 目录

控制解析类及方法时是否需要忽略的开关

每个文件用于控制以上一种场景，使用表达式语言方式配置：

```
parse_ignore_class.av
parse_ignore_method.av
```

### 2.3.5. _javacg2_parse_method_call_switch 目录

控制解析方法调用时是否需要忽略的开关，包括仅通过被调用方法判断、仅通过调用方法判断、通过调用方法与被调用方法判断

每个文件用于控制以上一种场景，使用表达式语言方式配置：

```
parse_ignore_method_call_ee.av
parse_ignore_method_call_er.av
parse_ignore_method_call_er_ee.av
```

### 2.3.6. 表达式语言配置示例

以上表达式语言配置示例可参考项目中的文件 src/main/resources/el_example.md

## 2.4. 配置参数-通过代码指定

在代码中使用 com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper 类可以指定配置参数

在创建 com.adrninistrator.javacg2.entry.JavaCG2Entry 类实例时需要有参数的构造函数“JavaCG2Entry(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper)”

示例如下：

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

# 3. 生成可以直接执行的文件

## 3.1. 编译方式

执行以下命令

```
gradlew gen_run_jar
```

## 3.2. 执行方式

执行以上命令后，会在`jar_output_dir`目录中生成可以直接执行的文件

在 Windows/Linux 等操作系统中分别执行对应的脚本文件`run.bat`、`run.sh`

# 4. 输出文件格式

[输出文件格式](docs/file_format.md)

# 5. 方法调用类型

[方法调用类型](docs/call_type.md)

# 6. 扩展功能

参考 [https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md) 中的相关内容

# 7. 更新说明

[更新说明](docs/change_log.md)

# 8. 原始 java-callgraph 调用关系缺失的场景

原始 java-callgraph 在多数场景下能够获取到 Java 方法调用关系，但以下场景的调用关系会缺失：

- 接口与实现类方法

假如存在接口 Interface1，及其实现类 Impl1，若在某个类 Class1 中引入了接口 Interface1，实际为实现类 Impl1 的实例（使用 Spring 时的常见场景），在其方法 Class1.func1() 中调用了 Interface1.fi() 方法；

原始 java-callgraph 生成的方法调用关系中，只包含 Class1.func1() 调用 Interface1.fi() 的关系，Class1.func1() 调用 Impl1.fi()，及 Impl1.fi() 向下调用的关系会缺失。

- Runnable 实现类线程调用

假如 f1() 方法中使用内部匿名类形式的 Runnable 实现类在线程中执行操作，在线程中执行了 f2() 方法，如下所示：

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

假如 f1() 方法中使用 lambda 表达式的形式在线程中执行操作，在线程中执行了 f2() 方法，如下所示：

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
