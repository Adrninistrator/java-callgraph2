# 1. 方法调用类型

|类型|描述|
|---|---|
|VIR|JVM的原始方法指令: INVOKEVIRTUAL|
|INT|JVM的原始方法指令: INVOKEINTERFACE|
|SPE|JVM的原始方法指令: INVOKESPECIAL|
|STA|JVM的原始方法指令: INVOKESTATIC|
|DYN|JVM的原始方法指令: INVOKEDYNAMIC|
|_SPR_ACT_I|被调用接口为 Spring Bean ，替换为实际的实现类类型|
|_SPR_ACT_C|被调用类为 Spring Bean ，替换为实际的子类类型|
|_ACT_I|被调用接口替换为实际的实现类类型|
|_ACT_C|被调用类替换为实际的子类类型|
|_ITF|接口调用实现类对应的方法|
|_LM|Lambda表达式|
|_RIR1|其他方法调用Runnable 构造函数|
|_RIR2|Runnable 构造函数调用 run() 方法|
|_CIC1|其他方法调用Callable 构造函数|
|_CIC2|Callable 构造函数调用 call() 方法|
|_TCID1|其他方法调用 TransactionCallback 构造函数|
|_TCID2|TransactionCallback 构造函数调用 doInTransaction() 方法|
|_TCWRID1|其他方法调用 TransactionCallbackWithoutResult 构造函数|
|_TCWRID2|TransactionCallbackWithoutResult 构造函数调用 doInTransactionWithoutResult() 方法|
|_TSR|Thread start() 方法调用 run() 方法|
|_SCC|父类调用子类对应的方法|
|_CCS|子类调用父类对应的方法|
|_CCS_SPE|子类通过super.调用父类方法|
|_CCS_I|子接口调用父接口对应的方法|
|_MA|人工添加的方法调用|
|_MAA|通过方法注解添加的调用关系|
