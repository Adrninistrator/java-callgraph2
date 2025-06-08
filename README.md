[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. `todo`配置文件    特性

# 2. DeepWiki 链接

[https://deepwiki.com/Adrninistrator/java-callgraph2](https://deepwiki.com/Adrninistrator/java-callgraph2)

通过大模型分析项目代码，可向大模型提出关于项目的问题，包括使用方法等

# 3. 项目说明

java-callgraph2 项目用于对 Java 代码（编译后的 class、jar、war 文件）进行静态分析，支持输出以下文件

|序号|文件内容|
|---|---|
|1|类的注解|
|2|类的继承或实现的泛型信息|
|3|类的信息|
|4|引用的类|
|5|类的签名中的泛型信息|
|6|重复类的信息|
|7|重复类的方法的信息|
|8|枚举类构造函数参数与字段赋值关系|
|9|枚举类初始化赋值信息|
|10|继承与实现相关信息|
|11|字段的注解|
|12|非静态字段集合中涉及的泛型类型|
|13|字段信息|
|14|通过get/set方法关联的字段关系|
|15|dto的get方法及字段|
|16|内部类信息|
|17|jar包信息|
|18|java-callgraph2组件使用的配置参数|
|19|Lambda表达式方法信息|
|20|日志-方法处理耗时|
|21|方法的注解|
|22|方法参数的注解|
|23|方法参数集合中涉及的泛型类型|
|24|方法参数|
|25|方法调用|
|26|方法调用的信息|
|27|方法调用使用方法调用返回值|
|28|方法调用使用静态字段信息|
|29|方法调用使用非静态字段信息|
|30|方法调用使用静态字段方法调用返回值|
|31|方法的catch信息|
|32|方法的finally信息|
|33|方法的信息|
|34|方法代码行号|
|35|方法返回值对应的方法序号|
|36|方法返回值对应的方法调用ID|
|37|方法返回的常量值（含null）|
|38|方法返回的字段（含枚举）|
|39|方法返回集合中涉及的泛型类型|
|40|方法通过throw抛出的异常信息|
|41|dto的set方法及字段|
|42|static、final字段初始化方法信息（含枚举）|
|43|Spring Bean信息|

当前项目原本 fork 自 [https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)，用于生成Java方法调用关系

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供通过静态分析获取 Java 方法调用关系的功能，因此创建了该项目

当前项目只会输出静态分析结果到文件，不会写入数据库；假如需要将结果写入数据库进行后续分析，例如生成Java代码完整方法调用链、生成调用堆栈、分析jar文件方法修改影响范围等，可使用项目[https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

`当前项目提供了插件功能，可用于为 Java 代码自动生成 UML 时序图（文档未完成，暂未提交）`，可参考 [https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram](https://github.com/Adrninistrator/gen-java-code-uml-sequence-diagram)。

# 4. 特性说明

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

# 使用说明

见[使用说明](docs/how_to_use.md)

# 5. 支持输出的文件格式

具体文件格式见 [输出文件格式](docs/file_format.md) ，有部分文件需要使用 java-all-call-graph 组件时支持输出

# 6. 方法调用类型

[方法调用类型](docs/call_type.md)

# 7. 扩展功能

参考 [https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md](https://github.com/Adrninistrator/java-all-call-graph/blob/main/extensions.md) 中的相关内容

# 8. 更新说明

[更新说明](docs/change_log.md)
