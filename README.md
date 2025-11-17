[![Maven Central](https://img.shields.io/maven-central/v/com.github.adrninistrator/java-callgraph2.svg)](https://search.maven.org/artifact/com.github.adrninistrator/java-callgraph2/)

# 1. 项目说明

java-callgraph2 项目用于对 Java 代码（编译生成的 class、jar、war、jmod 等文件）进行静态分析，包括类、方法、字段、注解、泛型、方法调用、枚举等，支持生成的文件见 [生成文件说明](docs/file_desc.md)。将非结构化的Java代码解析为结构化数据，可基于这些数据实现对代码的自动化分析，也可做为基础数据提供给大模型进行分析

当前项目原本 fork 自 [https://github.com/gousiosg/java-callgraph](https://github.com/gousiosg/java-callgraph)，用于生成 Java 方法调用关系

后来进行了优化和增强，差别已比较大，不容易合并回原始项目中，且仅提供通过静态分析获取 Java 方法调用关系的功能，因此创建了该项目

当前项目只会输出静态分析结果到文件，不会写入数据库；假如需要将结果写入数据库进行后续分析，例如生成 Java 代码方法完整调用链、生成调用堆栈、JarDiff 分析 jar 文件方法修改影响范围等，可使用项目 [https://github.com/Adrninistrator/java-all-call-graph](https://github.com/Adrninistrator/java-all-call-graph)

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

# 3. 静态分析支持生成的文件

## 3.1. 支持生成的文件格式

生成的具体文件格式见 [生成文件格式](docs/file_format.md) ，有部分文件需要使用 java-all-call-graph 组件时支持生成

以下为会生成的文件包含的信息

## 3.2. jar 文件、类、方法、字段基本信息

被解析的 jar 文件、目录等信息

类、方法、字段的基本信息，包括方法参数、方法代码行号等

## 3.3. 继承与实现信息

子类与父类之间的继承关系、实现类与接口之间的实现关系

## 3.4. 内部类与外部类

内部类及对应的外部类

## 3.5. 方法调用

方法之间的调用关系，会记录对应的调用方法与被调用方法，具体的功能特性在后续内容会说明

### 3.5.1. 方法调用类型

生成的方法调用关系文件中的该当调用类型可参考 [方法调用类型](docs/call_type.md)

## 3.6. 方法调用中使用的常量值、变量类型等信息

在方法调用中，被调用对象及参数所使用常量值、变量类型等信息，对于 Java 代码静态分析有非常重要的价值

当前项目在解析方法调用指令时，会使用类似符号解析的方式，获得相关信息，生成到描述为“方法调用的信息”文件中，目前支持获取的信息如下

### 3.6.1. 常量值

|示例代码|被解析的元素|解析结果|
|---|---|---|
|System.out.println("ok");|参数 1|ok|
|System.out.println(123);|参数 1|123|
|"abc1".getBytes()|被调用对象|abc1|

解析结果的内容为常量值的字符串形式

### 3.6.2. 变量类型

- 示例代码

```java
FRCDtoA frcDtoA = new FRCDtoA();
FRCDtoB frcDtoB = new FRCDtoB();
org.springframework.beans.BeanUtils.copyProperties(frcDtoA, frcDtoB);
```

- 解析结果

对于以上 BeanUtils.copyProperties 方法，获取到参数 1、参数 2 的类型分别如下

```
test.callgraph.fieldrelationships.frc.FRCDtoA
test.callgraph.fieldrelationships.frc.FRCDtoB
```

解析结果的内容为类型的完整类名

### 3.6.3. 静态字段

- 示例代码

```java
System.out.println("ok");
```

- 解析结果

对于以上 out.println 方法，获取到被调用字段如下

```
java.lang.System:out
```

解析结果的内容为静态字段所在类名与字段名称

### 3.6.4. 非静态字段

- 示例代码

```java
private String stringField1 = "abc1";
private TestField2 testField2a = new TestField2();

private void f1() {
    System.out.println(stringField1);
    System.out.println(testField2a.data);
}
```

- 解析结果

对于以上两个 out.println 方法，获取到参数 1 对应的非静态字段分别如下

```
this:stringField1
test.callgraph.field.TestField2:data
```

解析结果的内容为非静态字段所在类名与字段名称

若为 this 开头代表当前实例的字段

### 3.6.5. 静态字段的方法调用返回值

- 示例代码

```java
private static final Logger logger = LoggerFactory.getLogger(TestField1.class);

System.out.println(logger.isDebugEnabled());
```

- 解析结果

对于以上 out.println 方法，参数 1 属于静态字段的方法调用返回值，解析结果如下

```
test.callgraph.field.TestField1:logger:org.slf4j.Logger:isDebugEnabled():boolean
```

解析结果的内容为静态字段所在类名、字段名称、方法名称与参数、方法返回值

### 3.6.6. 变量的名称

- 示例代码

```java
private void test1() {
    test2();

    MethodWithAnnotation methodWithAnnotation = new MethodWithAnnotation();
    methodWithAnnotation.test1();
}

private void test2() {
}
```

- 解析结果

对于以上 test2();、methodWithAnnotation.test1(); 方法，解析到被调用对象的名称分别如下

```
this
methodWithAnnotation
```

解析结果的内容为被调用对象或参考对应的变量名称

### 3.6.7. 方法调用返回的 call_id

- 示例代码

```java
MethodWithAnnotation methodWithAnnotation = new MethodWithAnnotation();
methodWithAnnotation.test1();

Object object1 = this.clone();
System.out.println(object1.hashCode());
```

- 解析结果

对于以上 methodWithAnnotation.test1(); 方法，解析到被调用对象属于其他方法调用的返回结果，对应的方法调用 ID 为 2（示例，下同），即 new MethodWithAnnotation(); 方法调用的 call_id，即方法调用的序号

对于以上 new MethodWithAnnotation(); 方法，解析到参数 1 属于其他方法调用的返回结果，对应的方法调用 ID 为 27，即 this.clone(); 方法调用的 call_id

解析结果的内容为，方法调用的被调用对象或参数使用的其他方法调用返回值对应的 call_id

### 3.6.8. 方法参数的序号

- 示例代码

```java
private void f1(String str1, int int1) {
    str1.getBytes();
    System.out.println(int1);
}
```

- 解析结果

对于以上 str1.getBytes(); 方法，解析到被调用对象属于当前方法的参数，参数序号为 1（参数序号从 1 开始）

对于以上 out.println(int1); 方法，解析到参数 1 属于当前方法的参数，参数序号为 2

解析结果的内容为，方法调用的被调用对象或参数使用的当前方法的参数的序号

## 3.7. 方法调用中使用的其他信息

以下内容会生成单独的文件，与“方法调用中使用的常量值、变量类型等信息”中的内容类似，不再重复说明

```
方法调用使用方法调用返回值
方法调用使用静态字段信息
方法调用使用静态字段方法调用返回值
方法调用使用非静态字段信息
```

### 3.7.1. 方法调用被调用对象的原始类型

在创建对象实例时，可将类型定义为父类的，但创建为子类类型的实例。这样在调用该对象的方法时，在生成的字节码中，被调用方法的对象类型是父类的而不是子类的。即编译时的某个对象类型属于父类类型，运行时该对象类型属于子类类型

在处理对应的方法调用时，会将被调用对象定义时的父类类型记录下来

```java
AbstractSuperClassA superClassA = new ChildClassA1();
superClassA.entryA();
```

例如以上代码，会记录 superClassA.entryA(); 方法的调用 ID，及 superClassA 变量定义时的父类类型 test.callgraph.extendcomplex.AbstractSuperClassA

## 3.8. 方法返回的信息

### 3.8.1. 方法返回值对应的参数序号

- 示例代码

```java
private String f1(String str1) {
    return str1;
}
```

- 解析结果

对于以上 f1 方法，解析到返回的是当前方法的参数，参数序号为 1

解析结果的内容为，方法返回当前方法参数的序号

### 3.8.2. 方法返回值对应的方法调用 ID

- 示例代码

```java
private String f1() {
    return System.getProperty("test");
}
```

- 解析结果

对于以上 f1 方法，解析到返回的是调用 System.getProperty 方法的返回值，方法调用 call_id 为 10（示例）

解析结果的内容为，方法返回其他方法调用的 call_id

### 3.8.3. 方法返回的字段（含枚举）

- 示例代码

```java
public class TestUseArray1 {
    public static final int[][] ARRAY2 = new int[][]{new int[]{1, 2}, new int[]{3, 4}};
}    

public class TestReturnArray1 {
    public static int[][] return2() {
        return TestUseArray1.ARRAY2;
    }
}

public enum DbStatementEnum {
    DSE_ILLEGAL("-", "-");

    private final String statement;
    
    public static DbStatementEnum getFromStatement(String statement) {
        for (DbStatementEnum dbStatementEnum : DbStatementEnum.values()) {
            if (dbStatementEnum.getStatement().equals(statement)) {
                return dbStatementEnum;
            }
        }
        return DbStatementEnum.DSE_ILLEGAL;
    }

    public String getStatement() {
        return statement;
    }
}
```

- 解析结果

对于 TestReturnArray1.return2 方法，解析到返回了字段，相关信息如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|静态字段/非静态字段|静态字段|
|字段是否属于当前对象|不属于当前对象|
|字段所在的类名|test.callgraph.array.TestUseArray1|
|字段类型|int[][]|
|字段数组维度|2|
|字段名称|ARRAY2|

对于 DbStatementEnum.getFromStatement 方法，解析到第二个 return 返回了字段，相关信息如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|静态字段/非静态字段|静态字段|
|字段是否属于当前对象|不属于当前对象|
|字段所在的类名|test.callgraph.enums.DbStatementEnum|
|字段类型|test.callgraph.enums.DbStatementEnum|
|字段数组维度|0|
|字段名称|DSE_ILLEGAL|

对于 DbStatementEnum.getStatement 方法，解析到返回了字段，相关信息如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|静态字段/非静态字段|非静态字段|
|字段是否属于当前对象|属于当前对象|
|字段所在的类名|test.callgraph.enums.DbStatementEnum|
|字段类型|java.lang.String|
|字段数组维度|0|
|字段名称|statement|

解析结果的内容如上表格

### 3.8.4. 方法返回的常量值（含 null）

- 示例代码

```java
private String f1() {
    return "ok";
}

private String f2() {
    return null;
}
```

- 解析结果

对于 f1 方法，解析到返回的是常量值，类型为 String，值为"ok"

对于 f1 方法，解析到返回的是 null

解析结果的内容为方法返回的常量值，包括返回 null 的情况

## 3.9. 方法中的异常处理信息

### 3.9.1. 方法的 catch 信息

略

### 3.9.2. 方法的 finally 信息

略

### 3.9.3. 方法通过 throw 抛出的异常信息

- 示例代码

|代码行号|代码内容|
|---|---|
|650|try {|
|651|&nbsp;&nbsp;&nbsp;&nbsp;int a = 1;|
|652|&nbsp;&nbsp;&nbsp;&nbsp;int b = 1 / (a - 1);|
|653|&nbsp;&nbsp;&nbsp;&nbsp;throw new RuntimeException("1");|
|654|} catch (ArithmeticException e) {|
|655|&nbsp;&nbsp;&nbsp;&nbsp;logger.error("error1 ", e);|
|656|&nbsp;&nbsp;&nbsp;&nbsp;throw new RuntimeException(e);|
|657|} catch (Exception e) {|
|658|&nbsp;&nbsp;&nbsp;&nbsp;logger.error("error2 ", e);|
|659|&nbsp;&nbsp;&nbsp;&nbsp;// 以下写法不会识别出有使用 catch 的异常对象|
|660|&nbsp;&nbsp;&nbsp;&nbsp;throw new RuntimeException(Objects.toString(e));|
|661|}|

- 解析结果

解析到以上代码中有三处 throw 抛出异常，信息分别如下所示：

|获取到的信息分类|获取到的信息值|
|---|---|
|throw 指令偏移量|17|
|throw 操作代码行号|653|
|throw 的异常类型|java.lang.RuntimeException|
|throw 的方法调用 ID|701|

|获取到的信息分类|获取到的信息值|
|---|---|
|throw 指令偏移量|38|
|throw 操作代码行号|656|
|throw 的异常类型|java.lang.RuntimeException|
|throw 的方法调用 ID|703|

|获取到的信息分类|获取到的信息值|
|---|---|
|throw 指令偏移量|62|
|throw 操作代码行号|660|
|throw 的异常类型|java.lang.RuntimeException|
|throw 的方法调用 ID|706|

## 3.10. dto 的 get、set 方法及对应字段

解析哪些方法属于 dto 的 get 方法或 set 方法，以及方法所对应的字段名称及类型

## 3.11. 字段相关信息

### 3.11.1. 使用其他类中字段的使用情况

- 示例代码

|代码行号|代码内容|
|---|---|
|56|private void test4() {|
|57|&nbsp;&nbsp;&nbsp;&nbsp;DbStatementEnum dbStatementEnum = DbStatementEnum.getFromStatement(String.valueOf(System.currentTimeMillis()));|
|58|&nbsp;&nbsp;&nbsp;&nbsp;if (dbStatementEnum == DbStatementEnum.DSE_SELECT) {|
|59|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;System.getenv("");|
|60|&nbsp;&nbsp;&nbsp;&nbsp;}|
|61||
|62|&nbsp;&nbsp;&nbsp;&nbsp;switch (dbStatementEnum) {|
|63|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;case DSE_SELECT:|
|64|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;System.out.println("1");|
|65|&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;break;|

- 解析结果

解析到以上代码中有四处使用了其他类中的字段，信息分别如下所示：

编译器对 switch 相关代码进行了特殊处理，case 语句使用的字段在匿名内部类中，解析到的代码行号不是实际的代码行号；switch 语句生成了一个代码中未指定的使用字段的指令

|获取到的信息分类|获取到的信息值|
|---|---|
|使用其他字段的方法|test.callgraph.branch.TestBranch2$1:<clinit>()|
|被使用的字段所在的类|test.callgraph.enums.DbStatementEnum|
|被使用的字段类型|test.callgraph.enums.DbStatementEnum|
|被使用的字段名称|DSE_SELECT|
|字段被使用的代码行号|62|
|字段是静态字段还是非静态字段|静态字段|
|字段被读取还是赋值|被读取|

|获取到的信息分类|获取到的信息值|
|---|---|
|使用其他字段的方法|test.callgraph.branch.TestBranch2:test4()|
|被使用的字段所在的类|test.callgraph.enums.DbStatementEnum|
|被使用的字段类型|test.callgraph.enums.DbStatementEnum|
|被使用的字段名称|DSE_SELECT|
|字段被使用的代码行号|58|
|字段是静态字段还是非静态字段|静态字段|
|字段被读取还是赋值|被读取|

|获取到的信息分类|获取到的信息值|
|---|---|
|使用其他字段的方法|test.callgraph.branch.TestBranch2:test4()|
|被使用的字段所在的类|test.callgraph.branch.TestBranch2$1|
|被使用的字段类型|int[]|
|被使用的字段名称|`$SwitchMap$test$callgraph$enums$DbStatementEnum`|
|字段被使用的代码行号|62|
|字段是静态字段还是非静态字段|静态字段|
|字段被读取还是赋值|被读取|

|获取到的信息分类|获取到的信息值|
|---|---|
|使用其他字段的方法|test.callgraph.branch.TestBranch2:test4()|
|被使用的字段所在的类|java.lang.System|
|被使用的字段类型|java.io.PrintStream|
|被使用的字段名称|out|
|字段被使用的代码行号|64|
|字段是静态字段还是非静态字段|静态字段|
|字段被读取还是赋值|被读取|

### 3.11.2. static、final 字段初始化方法信息（含枚举）

支持解析 static、final 字段在初始化时进行赋值的方法调用，获取对应的方法调用 call_id

例如对于以下字段，支持获取到初始化时进行赋值的方法调用“LoggerFactory.getLogger(ElHandler.class);”的 call_id

```java
private static final Logger logger = LoggerFactory.getLogger(ElHandler.class);
```

## 3.12. 枚举相关处理

### 3.12.1. 枚举常量的使用情况

```java
public enum DbStatementEnum {

    DSE_SELECT("select", "查询");

    private final String statement;
    private final String desc;
```

在以上示例代码中，DSE_SELECT 属于枚举常量

枚举常量与静态字段使用相同的方式进行了解析处理

#### 3.12.1.1. 枚举常量在方法调用中的使用情况

方法调用的被调用对象或方法参数中使用静态字段的情况，包含使用枚举常量的情况

方法调用中使用静态字段的方法调用返回值的情况，包括使用枚举常量的方法调用返回值的情况

#### 3.12.1.2. 枚举常量所有的使用情况

使用其他类中字段的使用情况中，包括了枚举常量所有的使用情况

### 3.12.2. 枚举类初始化赋值信息

- 示例代码

```java
public enum DbStatementEnum {

    DSE_SELECT("select", "查询");

    private final String statement;
    private final String desc;

    DbStatementEnum(String statement, String desc) {
        this.statement = statement;
        this.desc = desc;
    }
```

- 解析结果

解析到以上枚举中的常量在初始化时的赋值信息如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|枚举类名|test.callgraph.enums.DbStatementEnum|
|枚举常量名称|DSE_SELECT|
|枚举常量序号|0|
|枚举构造函数序号（从 3 开始）|3|
|枚举字段类型|java.lang.String|
|枚举字段初始化值|select|

|获取到的信息分类|获取到的信息值|
|---|---|
|枚举类名|test.callgraph.enums.DbStatementEnum|
|枚举常量名称|DSE_SELECT|
|枚举常量序号|0|
|被赋值的枚举字段对应构造函数序号（从 3 开始）|4|
|被赋值的枚举字段类型|java.lang.String|
|被赋值的枚举字段初始化值|查询|

### 3.12.3. 枚举类构造函数参数与字段赋值关系

- 示例代码

同上

- 解析结果

解析到以上枚举中的构造函数的参数与枚举字段的赋值关系如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|枚举类名|test.callgraph.enums.DbStatementEnum|
|枚举构造函数序号（从 3 开始）|3|
|被赋值的枚举字段类型|java.lang.String|
|被赋值的枚举字段名称|statement|

|获取到的信息分类|获取到的信息值|
|---|---|
|枚举类名|test.callgraph.enums.DbStatementEnum|
|枚举构造函数序号（从 3 开始）|4|
|被赋值的枚举字段类型|java.lang.String|
|被赋值的枚举字段名称|desc|

## 3.13. 注解相关信息（使用 java-all-call-graph 项目解析生成格式更友好）

对于类、方法、字段、方法参数上的注解，支持解析对应的注解类名，注解的属性类型、属性名称与属性类型

## 3.14. 泛型相关信息

对于以下场景，支持解析使用的泛型类型

### 3.14.1. 方法参数集合中涉及的泛型类型

- 示例代码

```java
private void setADT(RAR rar, List<AIV> aivList)
```

- 解析结果

解析到参数 2 的参数类型是 java.util.List，参数中的泛型类型是 test.callgraph.branch.dto.AIV

### 3.14.2. 方法返回集合中涉及的泛型类型

- 示例代码

```java
public static List<String> getPomeLineList() throws IOException {
```

- 解析结果

解析到方法返回类型为 java.util.List，方法返回类型中的泛型类型为 java.lang.String

### 3.14.3. 类的签名中的泛型信息

- 示例代码

```java
public interface Common2Mapper<S extends java.lang.String, T1 extends java.lang.String> extends BaseMapper<T1>, Base2Mapper<T1> {
```

- 解析结果

解析到以上接口签名中中有两个泛型信息，第一个泛型类型变量名称为 S，泛型的父类类名为 java.lang.String；第二个泛型类型变量名称为 T1，泛型的父类类名为 java.lang.String

### 3.14.4. 类的继承或实现的泛型信息

- 示例代码

```java
public class GenericClassImplSuper2b3 extends GenericAbstractSuper2<List<String[]>, Map<String, Map<Integer, byte[]>>> {
```

- 解析结果

解析到以上类在继承父类时有两个泛型信息

第一个泛型信息中出现的类型为 java.util.List、java.lang.String[]

第二个泛型信息中出现的类型为 java.util.Map、java.lang.String、java.util.Map、java.lang.Integer、byte[]

### 3.14.5. 非静态字段集合中涉及的泛型类型

- 示例代码

```java
private final Set<InputStream> inputStreams = Collections.newSetFromMap(new WeakHashMap<>());
```

- 解析结果

解析到以上非静态字段字段类型是 java.util.Set，泛型类型是 java.io.InputStream

## 3.15. Lambda 表达式

- 示例代码

```java
threadPoolTaskExecutor.execute(() -> springInterfaceA.test1());

Map<String, Double> map = new HashMap<>();
map.put("a", 1.0D);
map.put("b", 2.0D);
map.put("c", 3.0D);

boolean matches1 = map.values().stream().filter(Objects::nonNull).mapToDouble(value -> {
    System.out.println("mapToDouble1 " + value);
    return value;
}).anyMatch(d -> {
    System.out.println("anyMatch1 " + d);
    return d > 4.0D;
});
```

- 解析结果

对“threadPoolTaskExecutor.execute”方法解析到的 Lambda 表达式信息如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|Lambda 表达式被调用方法|java.lang.Runnable:run()|
|Lambda 表达式下一个被调用的方法|org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor:execute(java.lang.Runnable)|
|Lambda 表达式下一个被调用方法是否为 Stream|否|
|Lambda 表达式下一个被调用方法是否为 Stream 的 intermediate（中间）操作|否|
|Lambda 表达式下一个被调用方法是否为 Stream 的 terminal（终端）操作|否|

对“map.values().stream()”方法解析到的 Lambda 表达式信息如下：

|获取到的信息分类|获取到的信息值|
|---|---|
|Lambda 表达式被调用方法|java.util.function.Predicate:test(java.lang.Object)|
|Lambda 表达式下一个被调用的方法|java.util.stream.Stream:filter(java.util.function.Predicate)|
|Lambda 表达式下一个被调用方法是否为 Stream|是|
|Lambda 表达式下一个被调用方法是否为 Stream 的 intermediate（中间）操作|是|
|Lambda 表达式下一个被调用方法是否为 Stream 的 terminal（终端）操作|否|

|获取到的信息分类|获取到的信息值|
|---|---|
|Lambda 表达式被调用方法|java.util.function.ToDoubleFunction:applyAsDouble(java.lang.Object)|
|Lambda 表达式下一个被调用的方法|java.util.stream.Stream:mapToDouble(java.util.function.ToDoubleFunction)|
|Lambda 表达式下一个被调用方法是否为 Stream|是|
|Lambda 表达式下一个被调用方法是否为 Stream 的 intermediate（中间）操作|是|
|Lambda 表达式下一个被调用方法是否为 Stream 的 terminal（终端）操作|否|

|获取到的信息分类|获取到的信息值|
|---|---|
|Lambda 表达式被调用方法|java.util.function.DoublePredicate:test(double)|
|Lambda 表达式下一个被调用的方法|java.util.stream.DoubleStream:anyMatch(java.util.function.DoublePredicate)|
|Lambda 表达式下一个被调用方法是否为 Stream|是|
|Lambda 表达式下一个被调用方法是否为 Stream 的 intermediate（中间）操作|否|
|Lambda 表达式下一个被调用方法是否为 Stream 的 terminal（终端）操作|是|

## 3.16. MyBatis 相关信息（需要使用 java-all-call-graph 项目解析）

支持解析使用 MySql 语法的 SQL 语句

### 3.16.1. MyBatis 的 Entity 与数据库字段名信息

支持解析 MyBatis Entity 中的字段对应数据库的哪个字段及字段类型，对应的 MyBatis XML 的 resultMap ID

### 3.16.2. MyBatis 的 Entity 与 Mapper、表名

支持解析哪些类属于 MyBatis Entity，及对应的 MyBatis Mapper 类、数据库表名、MyBatis XML 文件

### 3.16.3. MyBatis XML 的 sql、Mapper 相关信息

支持解析 MyBatis XML 文件中各个元素的 ID、数据库操作类型（select、insert、update、delete）、格式化后的 SQL 语句（XML 元素已被替换）、MyBatis Mapper 类名、MyBatis XML 文件路径

### 3.16.4. MyBatis 的 XML 中 select 的字段信息

支持解析 MyBatis Mapper 类的每个方法查询了哪些表的哪些字段，以及对应的 MyBatis XML 文件路径

### 3.16.5. MyBatis 的 XML 中 update set 子句的字段信息

支持解析 MyBatis Mapper 类的每个执行 update 操作的方法的每个参数更新了哪些表的哪些字段，以及对应的 MyBatis XML 文件路径

### 3.16.6. MyBatis Mapper 方法操作的数据库表信息

支持解析 MyBatis Mapper 类中的每个方法中执行数据库操作类型（select、insert、update、delete）与对应的数据库表名，以及对应的 MyBatis XML 文件路径

### 3.16.7. MyBatis 的 XML 中 where 子句的字段信息

支持解析 MyBatis Mapper 类中的每个方法中执行数据库操作对应的数据库表名，where 子句中进行判断的数据库字段、判断操作（=、>、<、<=、>=、!=、<>等）、MyBatis Mapper 方法的参数名称，以及对应的 MyBatis XML 文件路径

### 3.16.8. MyBatis Mapper 方法写的数据库表信息

支持解析 MyBatis Mapper 类中的每个写操作方法中执行数据库操作类型（insert、update、delete）与对应的数据库表名，以及对应的 MyBatis XML 文件路径

## 3.17. Spring 相关信息（需要使用 java-all-call-graph 项目解析）

### 3.17.1. Spring AOP

支持解析 Spring AOP 的 advice 影响的方法，包括 Spring AOP advice 定义方式（XML 中定义、代码中定义）、XML 中定义的 aspect 的 ID 与方法名、advice 类型（after、afterReturning、afterThrowing、around、before 等）、XML 中的 pointcut-ref 名称、pointcut 表达式、aspect 的 Order 排序数值、advice 的完整方法、影响的完整方法等

### 3.17.2. Spring Bean

Java 代码中通过注解定义的 Spring Bean 不需要使用 java-all-call-graph 项目解析，支持解析 Spring Bean 的名称、Spring Bean 的类名、Spring Bean 定义时对应的注解类名、Spring Bean 定义时所在的类名

Spring XML 文件中定义的 Bean 需要使用 java-all-call-graph 项目解析，支持解析 Spring Bean 的名称、Spring Bean 的类名、在 XML 中定义时对应的 profile、在 XML 中定义时对应的文件路径

### 3.17.3. Spring Controller

支持解析 Spring Controller 对应的完整方法、用于显示的 URI、类上的注解 path 属性原始值、方法上的注解 path 属性原始值、注解类名等

### 3.17.4. Spring Task

支持解析 Spring Task 对应的 Spring Bean 的名称、完整方法、在 Java 代码中定义时所在的类名、在 XML 中定义时对应的文件路径

### 3.17.5. Spring 包扫描路径

支持解析 Spring 包扫描路径，包括 XML 或 Java 代码中定义的

## 3.18. properties 文件内容（需要使用 java-all-call-graph 项目解析）

支持解析 properties 文件内容，包括 properties 配置名称、内容，以及 properties 文件路径

# 4. 方法调用解析功能特性

由于 Java 语言存在继承、多态、动态方法调用指令​​等特性，部分运行时会执行的方法调用关系在编译生成的字节码中不存在对应的指令，假如不进行对应处理，则这部分相关的调用关系会缺失，在当前项目中对以下场景进行了专门的处理

## 4.1. 继承相关的方法调用-父类调用子类方法

- 问题

接口中未实现的方法，在运行时会调用实现类中实现的方法；父类中的抽象方法，在运行时会调用子类中实现的方法

对于以上情况代码生成的字节码中，不会存在对应的接口方法调用实现类方法，或父类方法调用子类方法的指令

因此当前项目会添加接口未实现的方法调用实现类对应方法，父类抽象方法调用子类对应方法的调用关系

例如存在以下代码：

```java
public interface Interface1 {
    void f1();
}

public class ImplementClass1 implements Interface1 {
    @Override
    public void f1() {
        System.out.println("child1");
    }
}
```

- 处理

当前项目在解析方法调用关系时，会生成接口方法如以上 Interface1.f1() 调用实现类方法如以上 ImplementClass1.f1() 的调用关系

以上处理相对比较粗粒度，可能添加实际不存在的方法调用关系。因此当前项目还会对涉及多态的方法调用进行更准确的处理；使用当前项目生成的方法调用关系生成方法完整调用链的 java-all-call-graph 项目中，也会对涉及多态的调用链进行相关处理

## 4.2. 继承相关的方法调用-子类调用父类方法

- 问题

实现类/子类从接口/父类继承的，且实现类/子类中没有覆盖的方法，当实现类/子类运行时调用这些方法时，会调用接口/父类中的对应方法

对于以上情况代码生成的字节码中，会存在实现类调用方法调用当前类被调用方法的指令，以及子类调用方法调用当前类被调用方法的指令；但不会存在实现类被调用方法调用接口对应方法，或子类被调用方法调用父类对应方法的指令

因此当前项目会添加实现类调用从接口继承且未覆盖的方法，子类调用从父类继承且未覆盖的方法的调用关系

例如存在以下代码：

```java
public class SuperClass1 {
    protected void f1() {
        System.out.println("super1");
    }
}

public class ChildClass1 extends SuperClass1 {
    public void f2() {
        f1();
    }
}
```

以上 ChildClass1.f2 方法编译生成的字节码中，方法调用指令的被调用的方法是当前类 ChildClass1.f1()，不是父类方法 SuperClass1.f1()

- 处理

当前项目在解析方法调用关系时，会生成子类方法如以上 ChildClass1.f1() 调用父类方法如以上 SuperClass1.f1() 的调用关系

## 4.3. Runnable 实现类线程调用

- 问题

假如存在以下代码，在 f1 方法中使用匿名内部类形式的 Runnable 实现类实例，在线程中执行操作：

```java
private void f1() {
    new Thread(new Runnable() {
        @Override
        public void run() {
            System.out.println("in_thread");
        }
    }).start();
}
```

编译生成的字节码中，会存在 f1 方法调用 Runnable 实现类构造函数的调用关系，但不会存在 f1 方法调用 Runnable 实现类的 run 方法的调用关系。虽然以上操作是在两个不同的线程中执行的，但从调用关系和顺序来看，也可以认为属于调用方法与被调用方法。假如不进行对应的处理，则以上调用关系会缺失

对于使用命名类形式的 Runnable 实现类在线程中执行操作的情况，存在相同的问题

- 处理

当前项目在解析方法调用关系时，在处理到某方法调用 Runnable 实现类的构造函数时，会增加 Runnable 实现类的构造函数调用 run 方法的调用关系

以上处理准确的前提是，在某个方法创建 Runnable 对象后，在当前方法有通过该对象在线程中执行操作，在大部分情况下能够满足该要求

## 4.4. Callable 实现类线程调用

与 Runnable 实现类线程调用情况类似，区别在于 Callable 接口中是 call 方法，详细说明略

## 4.5. TransactionCallback 实现类事务处理

与 Runnable 实现类线程调用情况类似，区别在于 TransactionCallback 接口中是 doInTransaction 方法，且不涉及线程调用，详细说明略

## 4.6. TransactionCallbackWithoutResult 实现类事务处理

与 Runnable 实现类线程调用情况类似，区别在于 TransactionCallbackWithoutResult 接口中是 doInTransactionWithoutResult 方法，且不涉及线程调用，详细说明略

## 4.7. Thread 子类线程调用

- 问题

假如存在以下代码，在 f1 方法中使用匿名内部类形式的 Thread 子类实例，在线程中执行操作：

```java
private void f1() {
    new Thread() {
        @Override
        public void run() {
            System.getProperty("");
        }
    }.start();
}
```

编译生成的字节码中，会存在 f1 方法调用 Thread 子类 start 方法的调用关系，但不会存在 f1 方法调用 Thread 子类的 run 方法的调用关系

对于使用命名类形式的 Thread 子类在线程中执行操作的情况，存在相同的问题

- 处理

当前项目在解析方法调用关系时，在处理到某方法调用 Thread 子类的 start 方法时，会增加 Thread 子类的 start 方法调用 run 方法的调用关系

## 4.8. Lambda 表达式（含线程调用等）

- 问题

假如 f1 方法中使用 Lambda 表达式在线程中执行操作，或者执行其他操作，如下所示

```java
private void f1() {
    new Thread(() -> System.out.println("in_thread")).start();
    Assert.assertThrows(ArithmeticException.class, () -> {
        int a = 1;
        int b = 1 / (1 - a);
    });
}
```

编译生成的字节码中，Lambda 表达式对应代码会生成有 Synthetic 标志的代表 Lambda 表达式的方法，方法名格式为`lambda$所在方法名$序号`，如`lambda$f1$0`。会存在 Lambda 表达式方法向下调用其他方法的指令，但 Lambda 表达式所在方法（如以上 f1）调用 Lambda 表达式方法的指令是动态方法调用指令，无法直接根据该方法调用指令获取被调用的方法（即 Lambda 表达式方法）

- 处理

当前项目在解析方法调用关系时，当被调用方法属于 Lambda 表达式方法时，会增加一条方法调用关系，代表 Lambda 表达式所在方法调用 Lambda 表达式方法

## 4.9. Stream 中的方法引用

- 问题

在使用 Stream 时，xxx::func 形式的方法引用的字节码指令也是动态方法调用指令，与 Lambda 表达式的情况类似，区别在于 Lambda 表达式会生成对应的方法，Stream 使用的方法引用是已存在的方法

- 处理

当前项目在解析方法调用关系时，当被调用方法属于 Stream 的方法引用时，会增加一条方法调用关系，代表 Stream 的方法引用所在方法调用 Stream 的方法引用

## 4.10. 多态​​-对象创建及调用

- 问题

在创建对象实例时，可将类型定义为父类的，但创建为子类类型的实例。这样在调用该对象的方法时，在生成的字节码中，被调用方法的对象类型是父类的而不是子类的。即编译时的某个对象类型属于父类类型，运行时该对象类型属于子类类型

例如以下方法生成的字节码中，obj.f1() 对应的方法调用指令中的 obj 对象类型为父类 SuperClass1，而不是子类 ChildClass1：

```java
SuperClass1 obj = new ChildClass1();
obj.f1();
```

在以上使用多态的直接使用字节码中方法调用指令，假如不进行对应的处理，则获取到的被调用类型属于父类而不是子类，不符合预期

- 处理

假如对象定义为父类类型，将该对象实例化为子类类型后，在同一个方法中调用了该对象的方法，当前项目支持将被调用对象类型替换为子类类型

## 4.11. Spring Bean 字段注入类型替换

- 问题

Spring Bean 使用字段注入时，字段可以定义为接口/父类类型，实际注入的对象可以是实现类/子类类型

在这种情况下，根据生成的字节码的方法调用指令获得方法调用关系时，会导致被调用对象类型与实际情况不符

- 处理

当前项目在解析被调用对象属于 Spring Bean 的方法调用关系时，会将被调用对象类型替换为实际会注入的 Bean 类型

支持处理以下方式定义的 Bean：

```
在 Spring XML 文件中定义的 Bean
在类上使用@Component、@Controller、@Repository、@Service、@RestController、@Named 等注解定义的 Bean
在方法上使用@Bean 注解定义的 Bean
```

# 5. 总体步骤

当前项目在对 Java 代码进行静态分析时，总体的步骤如下

- 处理需要解析的文件
- 解析合并后的或原始 jar 文件
- 遍历并解析 jar 文件中的类
- 解析结果输出到文件

# 6. 对需要解析文件的处理

## 6.1. 支持解析的文件格式

支持对以下文件进行解析

|文件类型|说明|
|---|---|
|class|在解析时需要指定 class 文件所在的目录|
|jar|支持解析 jar 文件中的 class 等文件，及 jar 文件中的 jar 文件（Spring Boot 编译生成的 jar）中的文件|
|war|支持解析 war 文件中的 class 等文件，及 war 文件中的 jar 文件中的文件|
|jmod|JDK9 及以上的 JDK 标准库文件格式|
|xml|支持解析 Spring、MyBatis 相关的 XML 数据（需要通过 java-all-call-graph 实现）|
|properties|解析 properties 文件内容|

## 6.2. 直接使用原始 jar 文件

假如指定需要解析的文件是指定了一个 jar 文件，且该 jar 文件中不再存在 jar 文件，则会直接使用原始 jar 文件进行解析

## 6.3. 对指定需要解析的文件、目录进行合并

### 6.3.1. 需要合并的情况

在以下情况下，会将指定需要解析的文件、目录（中的文件）合并为一个 jar 文件

```
指定的需要解析的文件多于一个
指定的需要解析的内容包含目录
指定的需要解析的是 war 文件且其中包含 jar 文件
指定的需要解析的是 jar 文件且其中包含 jar 文件
指定的需要解析的是 jmod 文件
```

### 6.3.2. 合并生成的 jar 文件保存目录

若指定的需要解析的第一个元素为 jar、war、jmod 文件，则新生成的 jar 文件生成在同一个目录中

若指定的需要解析的第一个元素为目录，则新生成的 jar 文件生成在该目录中

文件名在第一个元素名称之后增加“-javacg2_merged.jar”作为后缀

### 6.3.3. 合并生成的 jar 文件结构

合并生成的 jar 文件的第一层目录名为格式为“{序号}@{原 jar、war、jmod 文件或目录名}”，如“0001@test.jar”

### 6.3.4. multi-release JAR 文件处理

假如需要解析的 jar 文件包含 multi-release JAR，则需要在_javacg2_config/config.properties 配置文件中指定参数 jdk.runtime.major.version，在合并 multi-release JAR 时使用指定版本的 class 文件，即“META-INF/versions/{JDK 主版本号}”目录中的 class 文件

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

### 7.2.3. 配置参数使用说明

参考 [配置参数使用说明](docs/_javacg2_all_config.md)

### 7.2.4. 重要配置参数

java-callgraph2 需要使用的重要配置参数是配置文件 _javacg2_config/jar_dir.properties

用于指定需要解析的 jar、war、jmod 文件路径，或保存 class、jar、war、jmod 文件的目录路径

对于使用 IDE 打开的 Java 项目，可以指定保存编译生成的 class 文件目录进行解析。例如使用 IDEA 时，可以指定 out 或目录 out/production/classes

### 7.2.5. 表达式作用

通过表达式可以指定忽略处理一些数据，具体用法参考配置参数使用说明

在合并生成 jar 文件时，支持通过表达式忽略部分文件

在解析类时，支持通过表达式忽略部分类

在解析方法时，支持通过表达式忽略部分方法

在解析方法调用时，支持通过表达式忽略部分方法调用

在解析方法调用被调用对象和参数可能的类型与值，支持通过表达式忽略部分方法

在处理 XML 文件中定义的 Spring Bean 时，支持通过表达式忽略部分 Bean

### 7.2.6. 表达式使用通用说明文档

参考 [表达式使用通用说明文档](src/main/resources/_el_example/el_usage.md)

### 7.2.7. 表达式字符串比较说明文档

参考 [表达式字符串比较说明文档](src/main/resources/_el_example/string_compare.md)

### 7.2.8. 通过代码指定配置参数

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
使用项目源码构建后运行
```

### 7.3.3. 各种运行方式支持的参数配置方式

|运行方式|支持的参数配置方式|
|---|---|
|通过 IDE 打开项目源码运行|通过配置文件指定<br>通过代码指定|
|在其他项目中引用当前项目的库运行|通过配置文件指定<br>通过代码指定|
|使用项目源码构建后运行|通过配置文件指定|

### 7.3.4. 通过 IDE 打开项目源码运行

通过 IDE 打开当前项目，由 Gradle 管理依赖库，可使用源码运行

#### 7.3.4.1. 通过配置文件指定参数运行

假如需要通过配置文件指定参数，可修改项目中的配置文件相关，再运行项目入口类

项目运行模块需要选择 test，以使 test 模块中的 log4j2 配置文件生效

#### 7.3.4.2. 通过代码指定参数运行

假如需要通过代码指定参数，可直接执行后续说明的示例方法，或者参考示例方法进行修改

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

最新版本号可查看 [https://mvnrepository.com/artifact/com.github.adrninistrator/java-callgraph2](https://mvnrepository.com/artifact/com.github.adrninistrator/java-callgraph2)

#### 7.3.5.2. 通过配置文件指定参数运行

假如需要通过配置文件指定参数，则需要将 java-callgraph2 项目的 src/main/resources 目录中以`_javacg2_`开头的目录复制到其他项目的 src/main/resources 或 src/test/resources 目录

修改配置文件相关参数后，可运行入口类

#### 7.3.5.3. 通过代码指定参数运行

与“通过 IDE 打开项目源码运行”的说明相同

### 7.3.6. 使用项目源码构建后运行

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
|log_javacg2|保存日志文件目录，运行后会生成|
|run.bat|用于执行当前项目解析 Java 代码的脚本|
|run.sh|用于执行当前项目解析 Java 代码的脚本|

#### 7.3.6.3. 通过配置文件指定参数运行

对配置文件参数进行配置后，可执行 run.bat 或 run.sh 脚本，调用项目的入口类，解析指定的 Java 代码

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

# 8. 人工增加方法调用关系

对于某些方法调用关系，不存在对应的方法调用指令，需要人工增加，可参考 java-all-call-graph 项目说明

# 9. 更新说明

[更新说明](docs/change_log.md)
