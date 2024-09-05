[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. 说明

java-callgraph2项目原本fork自[https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)。

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供通过静态分析获取Java方法调用关系的功能，因此创建了该项目。

`当前项目提供了插件功能，可用于为Java代码自动生成UML时序图`，可参考[https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)。

# 2. 使用说明

## 2.1. 通过源码编译

### 2.1.1. 编译方式

执行以下命令

```
gradlew jar
```

### 2.1.2. 执行方式

执行命令可参考脚本文件`run.bat`、`run.sh`

#### 2.1.2.1. 程序主类

```java
com.adrninistrator.javacg2.stat.JCallGraph2
```

#### 2.1.2.2. 配置参数

参考 _javacg2_config 目录的配置文件

主要配置文件为 config.properties

需要分析的jar包或目录路径对应的配置文件为 jar_dir.properties

假如仅分析特定包名的类，可在配置文件 packages.properties 中配置

## 2.2. 通过组件引用

可在其他项目中通过组件方式引用当前项目，添加的Gradle格式的组件依赖如下：

```
com.github.adrninistrator:java-callgraph2:
```

# 3. 生成可以直接执行的文件

## 3.1. 编译方式

执行以下命令

```
gradlew gen_run_jar
```

## 3.2. 执行方式

执行以上命令后，会在`output_dir`目录中生成可以直接执行的文件

在Windows/Linux等操作系统中分别执行对应的脚本文件`run.bat`、`run.sh`

# 4. 输出文件格式

[输出文件格式](docs/file_format.md)

[方法调用类型](docs/call_type.md)

# 5. 扩展功能

参考[https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md)中的相关内容

# 6. 更新说明

## 6.1. (0.1.2)

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

## 6.2. (0.1.3)

生成的注解信息文件中增加类上的注解信息，包含注解名称、注解属性名称及属性值；增加方法上的注解的注解属性名称及属性值

## 6.3. (0.1.5)

在对类进行处理时，跳过重复同名类

## 6.4. (0.1.7)

支持人工添加缺失的方法调用关系（定制化代码开发）

具体说明可参考[https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md)中的相关内容

## 6.5. (0.1.8)

生成方法代码行号信息文件，文件名以“-line_number.txt”结尾

## 6.6. (0.1.9)

对于某个类调用自身类的类调用关系，也会生成在输出文件中

## 6.7. (0.2.0)

在处理注解的属性值时，支持使用自定义类处理，自定义类需要实现`com.adrninistrator.javacg.extensions.annotation_attributes.AnnotationAttributesFormatorInterface`接口

可使用java-all-call-graph中的`com.adrninistrator.jacg.extensions.annotation_attributes.AllAnnotationAttributesFormator`类

## 6.8. (0.2.1)

对于接口之间的继承关系，在内存中进行记录

## 6.9. (0.2.2)

方法调用对象中增加字段

## 6.10. (1.0.0)临时说明

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

## 6.11. (1.0.16)临时说明

在输出的`method_call`文件中，增加增加调用方法返回类型，使被调用方法返回类型更准确

## 6.12. (2.0.7)临时说明

支持分析同名的jar包、目录

为了使代码及配置文件与“jacg”更容易区分，对代码及配置文件进行以下名称修改

|内容|修改前的名称|修改后的名称|
|---|---|---|
|包名|com.adrninistrator.javacg|com.adrninistrator.javacg2|
|主类|com.adrninistrator.javacg.stat.JCallGraph|com.adrninistrator.javacg2.entry.JavaCG2Entry|
|目录、文件名称|javacg|javacg2|
|类名前缀|JavaCG|JavaCG2|

`当升级到该版本时，假如之前有对目录进行过分析，则相关目录中的“-javacg_merged.jar”文件需要删除`

# 7. 原始java-callgraph调用关系缺失的场景

原始java-callgraph在多数场景下能够获取到Java方法调用关系，但以下场景的调用关系会缺失：

- 接口与实现类方法

假如存在接口Interface1，及其实现类Impl1，若在某个类Class1中引入了接口Interface1，实际为实现类Impl1的实例（使用Spring时的常见场景），在其方法Class1.func1()中调用了Interface1.fi()方法；

原始java-callgraph生成的方法调用关系中，只包含Class1.func1()调用Interface1.fi()的关系，Class1.func1()调用Impl1.fi()，及Impl1.fi()向下调用的关系会缺失。

- Runnable实现类线程调用

假如f1()方法中使用内部匿名类形式的Runnable实现类在线程中执行操作，在线程中执行了f2()方法，如下所示：

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

原始java-callgraph生成的方法调用关系中，f1()调用f2()，及f2()向下调用的关系会缺失；

对于使用命名类形式的Runnable实现类在线程中执行操作的情况，存在相同的问题，原方法调用线程中执行的方法，及继续向下的调用关系会缺失。

- Callable实现类线程调用

与Runnable实现类线程调用情况类似，略。

- Thread子类线程调用

与Runnable实现类线程调用情况类似，略。

- lambda表达式（含线程调用等）

假如f1()方法中使用lambda表达式的形式在线程中执行操作，在线程中执行了f2()方法，如下所示：

```java
private void f1() {
    new Thread(() -> f2()).start();
}
```

原始java-callgraph生成的方法调用关系中，f1()调用f2()，及f2()向下调用的关系会缺失；

对于其他使用lambda表达式的情况，存在相同的问题，原方法调用lambda表达式中执行的方法，及继续向下的调用关系会缺失。

- Stream调用

在使用Stream时，通过xxx::func方式调用方法，原始java-callgraph生成的方法调用关系中会缺失。如以下示例中，当前方法调用当前类的map2()、filter2()，及TestDto1类的getStr()方法的调用关系会缺失。

```java
list.stream().map(this::map2).filter(this::filter2).collect(Collectors.toList());
list.stream().map(TestDto1::getStr).collect(Collectors.toList());
```

- 父类调用子类的实现方法

假如存在抽象父类Abstract1，及其非抽象子类ChildImpl1，若在某个类Class1中引入了抽象父类Abstract1，实际为子类ChildImpl1的实例（使用Spring时的常见场景），在其方法Class1.func1()中调用了Abstract1.fa()方法；

原始java-callgraph生成的方法调用关系中，只包含Class1.func1()调用Abstract1.fa()的关系，Class1.func1()调用ChildImpl1.fa()的关系会缺失。

- 子类调用父类的实现方法

假如存在抽象父类Abstract1，及其非抽象子类ChildImpl1，若在ChildImpl1.fc1()方法中调用了父类Abstract1实现的方法fi()；

原始java-callgraph生成的方法调用关系中，ChildImpl1.fc1()调用Abstract1.fi()的关系会缺失。

针对以上问题，java-callgraph2都进行了优化，能够生成缺失的调用关系。

java-callgraph2地址为[https://github.com/Adrninistrator/java-callgraph2](https://github.com/Adrninistrator/java-callgraph2)。

对于更复杂的情况，例如存在接口Interface1，及其抽象实现类Abstract1，及其子类ChildImpl1，若在某个类中引入了抽象实现类Abstract1并调用其方法的情况，生成的方法调用关系中也不会出现缺失。
