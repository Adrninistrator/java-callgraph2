package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.method.MethodAndArgs;
import com.adrninistrator.javacg.extensions.code_parser.JarEntryOtherFileParser;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.spring.DefineSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarInputStream;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，第一次预处理
 */
public class JarEntryPreHandle1Parser extends AbstractJarEntryParser {

    private Map<String, ClassImplementsMethodInfo> classInterfaceMethodInfoMap;

    private Map<String, List<MethodAndArgs>> interfaceMethodWithArgsMap;

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;

    private Set<String> classExtendsSet;

    private Set<String> interfaceExtendsSet;

    private final ExtensionsManager extensionsManager;

    private final DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler;

    public JarEntryPreHandle1Parser(JavaCGConfInfo javaCGConfInfo, DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler, ExtensionsManager extensionsManager) {
        super(javaCGConfInfo);
        this.defineSpringBeanByAnnotationHandler = defineSpringBeanByAnnotationHandler;
        this.extensionsManager = extensionsManager;
    }

    @Override
    protected boolean handleEntry(JarInputStream jarInputStream, String jarEntryName) throws IOException {
        // 尝试处理jar包中的class文件
        if (tryHandleClassEntry(jarInputStream, jarEntryName)) {
            // 是class文件，不再处理
            return true;
        }

        // 非class文件，判断是否需要使用扩展类处理jar包中的文件
        String jarEntryFileExt = StringUtils.substringAfterLast(jarEntryName, JavaCGConstants.FLAG_DOT);
        List<JarEntryOtherFileParser> jarEntryOtherFileParserList = extensionsManager.getJarEntryOtherFileParserList(jarEntryFileExt);
        if (jarEntryOtherFileParserList == null) {
            // 当前文件不存在对应的扩展类，不处理
            return true;
        }

        // 存在扩展类需要处理当前文件
        // 将不可重复读的JarInputStream缓存为可以重复读取的ByteArrayInputStream
        InputStream cachedInputStream = JavaCGUtil.cacheInputStream(jarInputStream);
        if (cachedInputStream == null) {
            return false;
        }

        // 调用扩展类的方法
        for (JarEntryOtherFileParser jarEntryOtherFileParser : jarEntryOtherFileParserList) {
            // 处理一个jar包中的文件
            jarEntryOtherFileParser.parseJarEntryOtherFile(cachedInputStream, jarEntryName);
            // 重置缓存的InputStream，使下次能够从头开始继续读取
            cachedInputStream.reset();
        }

        return true;
    }

    @Override
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryName) {
        if (javaClass.isInterface()) {
            // 对一个接口进行预处理
            preHandle1Interface(javaClass);
            return true;
        }

        // 对一个类进行预处理
        preHandle1Class(javaClass);

        if (javaCGConfInfo.isParseMethodCallTypeValue()) {
            // 处理Spring Bean相关注解
            return defineSpringBeanByAnnotationHandler.recordSpringBeanInfo(javaClass);
        }

        return true;
    }

    // 对一个接口进行预处理
    private void preHandle1Interface(JavaClass interfaceClass) {
        if (interfaceClass.isAnnotation()) {
            // 不处理注解
            return;
        }

        String interfaceName = interfaceClass.getClassName();
        // 记录接口的方法
        Method[] methods = interfaceClass.getMethods();
        if (methods != null && methods.length > 0 &&
                interfaceMethodWithArgsMap.get(interfaceName) == null) {
            List<MethodAndArgs> interfaceMethodWithArgsList = JavaCGByteCodeUtil.genInterfaceMethodWithArgs(methods);
            interfaceMethodWithArgsMap.put(interfaceName, interfaceMethodWithArgsList);
        }

        String[] superInterfaceNames = interfaceClass.getInterfaceNames();
        if (superInterfaceNames.length > 0) {
            // 记录涉及继承的接口
            interfaceExtendsSet.add(interfaceName);
            interfaceExtendsSet.addAll(Arrays.asList(superInterfaceNames));
        }
    }

    // 对一个Class进行预处理
    private void preHandle1Class(JavaClass javaClass) {
        String className = javaClass.getClassName();
        String[] interfaceNames = javaClass.getInterfaceNames();
        Method[] methods = javaClass.getMethods();

        if (interfaceNames.length > 0 &&
                methods != null && methods.length > 0 &&
                classInterfaceMethodInfoMap.get(className) == null) {
            List<String> interfaceNameList = new ArrayList<>(interfaceNames.length);
            interfaceNameList.addAll(Arrays.asList(interfaceNames));

            List<MethodAndArgs> implClassMethodWithArgsList = JavaCGByteCodeUtil.genImplClassMethodWithArgs(methods);

            classInterfaceMethodInfoMap.put(className, new ClassImplementsMethodInfo(interfaceNameList, implClassMethodWithArgsList));

            if (!javaClass.isAbstract()) {
                if (interfaceNameList.contains(JavaCGCommonNameConstants.CLASS_NAME_RUNNABLE)) {
                    // 找到Runnable实现类
                    runnableImplClassMap.put(className, Boolean.FALSE);
                }
                if (interfaceNameList.contains(JavaCGCommonNameConstants.CLASS_NAME_CALLABLE)) {
                    // 找到Callable实现类
                    callableImplClassMap.put(className, Boolean.FALSE);
                }
                if (interfaceNameList.contains(JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK)) {
                    // 找到TransactionCallback实现类
                    transactionCallbackImplClassMap.put(className, Boolean.FALSE);
                }
            }
        }

        // 获得父类和子类信息
        String superClassName = javaClass.getSuperclassName();
        if (JavaCGCommonNameConstants.CLASS_NAME_THREAD.equals(superClassName)) {
            // 找到Thread的子类
            threadChildClassMap.put(className, Boolean.FALSE);
        } else if (JavaCGCommonNameConstants.CLASS_NAME_TIMER_TASK.equals(superClassName)) {
            // 找到TimerTask的子类，按照Runnable实现类处理
            runnableImplClassMap.put(className, Boolean.FALSE);
        }else if (JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT.equals(superClassName)) {
            // 找到TransactionCallbackWithoutResult实现类
            transactionCallbackWithoutResultChildClassMap.put(className, Boolean.FALSE);
        }

        if (!JavaCGUtil.isClassInJdk(superClassName)) {
            classExtendsSet.add(className);
            classExtendsSet.add(superClassName);
        }
    }

    public void setClassInterfaceMethodInfoMap(Map<String, ClassImplementsMethodInfo> classInterfaceMethodInfoMap) {
        this.classInterfaceMethodInfoMap = classInterfaceMethodInfoMap;
    }

    public void setInterfaceMethodWithArgsMap(Map<String, List<MethodAndArgs>> interfaceMethodWithArgsMap) {
        this.interfaceMethodWithArgsMap = interfaceMethodWithArgsMap;
    }

    public void setRunnableImplClassMap(Map<String, Boolean> runnableImplClassMap) {
        this.runnableImplClassMap = runnableImplClassMap;
    }

    public void setCallableImplClassMap(Map<String, Boolean> callableImplClassMap) {
        this.callableImplClassMap = callableImplClassMap;
    }

    public void setTransactionCallbackImplClassMap(Map<String, Boolean> transactionCallbackImplClassMap) {
        this.transactionCallbackImplClassMap = transactionCallbackImplClassMap;
    }

    public void setTransactionCallbackWithoutResultChildClassMap(Map<String, Boolean> transactionCallbackWithoutResultChildClassMap) {
        this.transactionCallbackWithoutResultChildClassMap = transactionCallbackWithoutResultChildClassMap;
    }

    public void setThreadChildClassMap(Map<String, Boolean> threadChildClassMap) {
        this.threadChildClassMap = threadChildClassMap;
    }

    public void setClassExtendsSet(Set<String> classExtendsSet) {
        this.classExtendsSet = classExtendsSet;
    }

    public void setInterfaceExtendsSet(Set<String> interfaceExtendsSet) {
        this.interfaceExtendsSet = interfaceExtendsSet;
    }
}
