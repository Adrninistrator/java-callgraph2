[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. 使用说明

java-callgraph2项目原本fork自[https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)。

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供静态分析的功能，因此创建了该项目。

`当前项目提供了插件功能，可用于为Java代码自动生成UML时序图`，可参考[https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)。

# 2. 更新说明

- 0.0.4

支持对目录中的class文件进行处理

支持将指定的一个或多个jar包或目录进行合并后再处理

提供用于生成Java方法UML时序图的插件功能

不再通过JVM参数“-Doutput.file=”指定输出文件路径，默认将输出文件生成在指定的第一个jar包所在目录中（若指定的是目录则在该目录中）

## 2.1. 编译命令：

```
gradlew jar
```

## 2.2. 执行参数

- Program arguments

用于指定需要解析的jar包路径列表

```
build/libs/a.jar build/libs/b.jar
```

# 3. 输出格式

java-callgraph2输出的方法调用关系的格式如下所示：

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

jar包序号，从1开始

# 4. 增加jar包文件路径

java-callgraph2会输出当前jar包文件路径，如下所示：

```
J:jar_number jar_file_path
```

- jar_number

jar包序号，从1开始

- jar_file_path

jar包文件路径

若为包含class文件的目录，则以上“J:”使用“D:”

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
