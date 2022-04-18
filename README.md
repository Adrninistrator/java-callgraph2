[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. 说明

java-callgraph2项目原本fork自[https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)。

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供通过静态分析获取Java方法调用关系的功能，因此创建了该项目。

`当前项目提供了插件功能，可用于为Java代码自动生成UML时序图`，可参考[https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)。

# 2. 更新说明

## 2.1. (0.1.2)

- 支持对目录进行处理

支持对目录中的class、jar/war文件进行处理

支持在启动参数中指定一个或多个jar/war包或目录

在处理时，会将指定的jar/war包及指定目录合并为一个新的jar包后再处理：

a. 对于指定的jar/war包及指定目录中的后缀为.jar/.war的文件，将其中的class文件进行合并

b. 对于指定目录中的后缀非.jar/.war的文件进行合并

合并jar/war包中的class文件时，支持仅将指定包名的class文件合并到新的jar包中

合并产生的新jar包信息如下：

保存在指定的第一个jar/war包所在目录中（若第一个是目录则在该目录中）

文件名为第一个jar/war包加上“-javacg_merged.jar”

第一层目录为每个被合并的.jar/.war文件或目录的名称

- 支持插件功能

提供用于生成Java方法UML时序图的插件功能

- 输出文件路径指定方式变化

不再通过JVM选项“-Doutput.file=”指定输出文件路径，默认将输出文件生成在指定的第一个jar包所在目录中（若第一个是目录则在该目录中）

方法调用关系文件名为第一个jar包或合并后的jar包加上“.txt”

注解信息文件名为第一个jar包或合并后的jar包加上“-annotation.txt”

## 2.2. (0.1.3)

生成的注解信息文件中增加类上的注解信息，包含注解名称、注解属性名称及属性值；增加方法上的注解的注解属性名称及属性值

## 2.3. (0.1.5)

在对类进行处理时，跳过重复同名类

# 3. 使用说明

## 3.1. 编译命令：

```
gradlew jar
```

## 3.2. 执行参数

### 3.2.1. 程序参数（Program arguments）

用于指定需要解析的jar/war包或目录路径列表，支持指定一个或多个jar包或目录的路径（指定目录时，会处理其中的class或jar文件）

当指定目录时，或指定多个jar文件时，会合并成一个jar包后再处理，文件名为第一个jar/war包加上“-javacg_merged.jar”


示例如下：

```
out
build/libs/a.jar
build/libs/a.jar build/libs/b.jar
out build/libs/a.jar build/libs/b.jar
```

### 3.2.2. JVM选项（VM options）

- merge.class.in.jar.package

当前参数用于指定合并jar/war包中的class文件时，需要合并的包名

假如未指定当前参数，或当前参数值为空，则合并jar/war包中的class文件时，对所有的class文件都进行合并

假如当前参数值非空，则合并jar/war包中的class文件时，仅对包名满足该参数值的class文件进行合并（即包名不满足该参数值的class文件不合并到新的jar包中）

当前参数值支持指定一个或多个需要合并的包名，多个包名之间使用“|”分隔

```
-Dmerge.class.in.jar.package=aa.bb.cc
-Dmerge.class.in.jar.package=aa.bb.cc|tt.cc.ss|cc.mm.
```

# 4. 输出文件格式

## 4.1. 方法调用关系文件

方法调用关系文件生成在指定的第一个jar包所在目录中（若第一个是目录则在该目录中），文件名为第一个jar包或合并后的jar包加上“.txt”

文件各字段之间使用空格作为分隔符

### 4.1.1. jar包/目录信息

jar包/目录信息以“J:”或“D:”开头，格式如下所示：

```
J:jar_number jar包文件绝对路径
```

java-callgraph2输出的目录信息格式如下所示：

```
D:jar_number 目录绝对路径
```

以上jar_number代表当前jar包或目录的唯一序号，由于可能存在重复文件不会处理，因此该序号不一定连续

`某个jar包/目录信息到下一个jar包/目录信息或文件结尾之间的类引用关系及方法调用关系，代表存在于当前的jar包或目录中`

### 4.1.2. 类引用关系

类引用关系以“C:”开头，格式如下所示：

```
C:caller_class callee_class
```

- caller_class

代表当前类的完整类名

- callee_class

代表被当前类引用的类的完整类名

### 4.1.3. 方法调用关系

方法调用关系以“M:”开头，格式如下所示：

```
M:call_id class1:<method1>(arg_types) (typeofcall)class2:<method2>(arg_types) line_number jar_number
```

- call_id

代表当前方法调用的ID

- typeofcall

原始java-callgraph支持的调用类型typeofcall如下：

|typeofcall|含义|
|---|---|
|M|invokevirtual|
|I|invokeinterface|
|O|invokespecial|
|S|invokestatic|
|D|invokedynamic|

java-callgraph2增加的调用类型typeofcall如下：

|typeofcall|含义|
|---|---|
|ITF|接口与实现类方法|
|RIR|Runnable实现类线程调用|
|CIC|Callable实现类线程调用|
|TSR|Thread子类线程调用|
|LM|lambda表达式（含线程调用等）|
|ST|Stream调用|
|SCC|父类调用子类的实现方法|
|CCS|子类调用父类的实现方法|

- line_number

为当前调用者方法源代码对应行号

- jar_number

jar包唯一序号，从1开始

## 4.2. 注解信息文件

注解信息文件生成在指定的第一个jar包所在目录中（若第一个是目录则在该目录中），文件名为第一个jar包或合并后的jar包加上“-annotation.txt”

文件各字段之间使用空格作为分隔符

文件格式如下：

```
type class_or_method_name annotation_name annotation_attribute_name annotation_attribute_value
```

- type

“C:”代表为类上的注解信息

“M:”代表为方法上的注解信息

- class_or_method_name

当前行为类上的注解信息时，该字段值为完整类名

当前行为方法上的注解信息时，该字段值为完整方法名（完整类名+方法名+方法参数）

- annotation_name

注解的完整类名

- annotation_attribute_name

注解属性的名称，可能为空

- annotation_attribute_value

注解属性的值，可能为空

属性值中的空格会被替换为0x01

假如属性值类型为数组，则属性值会被大括号包含，如“{test}”

若注解没有属性值，则只有以上前三个字段，占一行

若注解有属性值，则有以上五个字段，每个属性占一行

示例如下：

```
C: com.test.controller.TestLoaderController org.springframework.stereotype.Controller
C: com.test.controller.TestLoaderController org.springframework.web.bind.annotation.RequestMapping value {test}
M: com.test.controller.TestRest2Controller:get(javax.servlet.http.HttpServletRequest) org.springframework.web.bind.annotation.GetMapping value {get}
M: com.test.controller.TestRest2Controller:get(javax.servlet.http.HttpServletRequest) com.test.common.annotation.TestAttributeAnnotation value abc
M: com.test.controller.TestRest2Controller:get(javax.servlet.http.HttpServletRequest) com.test.common.annotation.TestAttributeAnnotation value2 123
```

# 5. 原始java-callgraph调用关系缺失的场景

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
