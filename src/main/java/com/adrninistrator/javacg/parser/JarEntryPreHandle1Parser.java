package com.adrninistrator.javacg.parser;

import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.conf.JavaCGConfInfo;
import com.adrninistrator.javacg.dto.classes.ClassImplementsMethodInfo;
import com.adrninistrator.javacg.dto.jar.ClassAndJarNum;
import com.adrninistrator.javacg.dto.jar.JarInfo;
import com.adrninistrator.javacg.dto.method.MethodArgReturnTypes;
import com.adrninistrator.javacg.extensions.codeparser.JarEntryOtherFileParser;
import com.adrninistrator.javacg.extensions.manager.ExtensionsManager;
import com.adrninistrator.javacg.spring.DefineSpringBeanByAnnotationHandler;
import com.adrninistrator.javacg.util.JavaCGByteCodeUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/9/14
 * @description: 解析jar包中的文件，第一次预处理
 */
public class JarEntryPreHandle1Parser extends AbstractJarEntryParser {

    private static final Logger logger = LoggerFactory.getLogger(JarEntryPreHandle1Parser.class);

    private Map<String, ClassImplementsMethodInfo> classImplementsMethodInfoMap;

    private Map<String, List<MethodArgReturnTypes>> interfaceMethodWithArgTypesMap;

    private Map<String, Boolean> runnableImplClassMap;
    private Map<String, Boolean> callableImplClassMap;
    private Map<String, Boolean> transactionCallbackImplClassMap;
    private Map<String, Boolean> transactionCallbackWithoutResultChildClassMap;
    private Map<String, Boolean> threadChildClassMap;
    private Set<String> allClassNameSet;

    private Map<String, String> classAndSuperMap;

    private Set<String> interfaceExtendsSet;

    private ClassAndJarNum classAndJarNum;

    private final ExtensionsManager extensionsManager;

    private final DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler;

    public JarEntryPreHandle1Parser(JavaCGConfInfo javaCGConfInfo, Map<String, JarInfo> jarInfoMap, DefineSpringBeanByAnnotationHandler defineSpringBeanByAnnotationHandler,
                                    ExtensionsManager extensionsManager) {
        super(javaCGConfInfo, jarInfoMap);
        this.defineSpringBeanByAnnotationHandler = defineSpringBeanByAnnotationHandler;
        this.extensionsManager = extensionsManager;
    }

    @Override
    protected boolean handleEntry(ZipInputStream zipInputStream, String jarEntryPath) throws IOException {
        // 尝试处理jar包中的class文件
        if (tryHandleClassEntry(zipInputStream, jarEntryPath)) {
            // 是class文件，不再处理
            return true;
        }

        // 非class文件，判断是否需要使用扩展类处理jar包中的文件
        String jarEntryFileExt = JavaCGConstants.FLAG_DOT + StringUtils.substringAfterLast(jarEntryPath, JavaCGConstants.FLAG_DOT);
        logger.debug("jar包中文件的后缀: [{}] {}", jarEntryPath, jarEntryFileExt);
        List<JarEntryOtherFileParser> jarEntryOtherFileParserList = extensionsManager.getJarEntryOtherFileParserList(jarEntryFileExt);
        if (jarEntryOtherFileParserList == null) {
            // 当前文件不存在对应的扩展类，不处理
            logger.debug("当前文件不存在对应的扩展类，不处理 {}", jarEntryFileExt);
            return true;
        }

        // 存在扩展类需要处理当前文件
        // 将不可重复读的JarInputStream缓存为可以重复读取的ByteArrayInputStream
        InputStream cachedInputStream = JavaCGUtil.cacheInputStream(zipInputStream);
        if (cachedInputStream == null) {
            return false;
        }

        // 调用扩展类的方法
        for (JarEntryOtherFileParser jarEntryOtherFileParser : jarEntryOtherFileParserList) {
            // 处理一个jar包中的文件
            jarEntryOtherFileParser.parseJarEntryOtherFile(cachedInputStream, jarEntryPath);
            // 重置缓存的InputStream，使下次能够从头开始继续读取
            cachedInputStream.reset();
        }

        return true;
    }

    @Override
    protected boolean handleClassEntry(JavaClass javaClass, String jarEntryPath) {
        // 记录类名及所在的jar包序号
        classAndJarNum.put(javaClass.getClassName(), lastJarInfo.getJarNum());

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
                interfaceMethodWithArgTypesMap.get(interfaceName) == null) {
            List<MethodArgReturnTypes> interfaceMethodWithArgTypesList = JavaCGByteCodeUtil.genInterfaceMethodWithArgTypes(methods);
            interfaceMethodWithArgTypesMap.put(interfaceName, interfaceMethodWithArgTypesList);
        }

        String[] superInterfaceNames = interfaceClass.getInterfaceNames();
        if (superInterfaceNames.length > 0) {
            // 记录涉及继承的接口
            interfaceExtendsSet.add(interfaceName);
            interfaceExtendsSet.addAll(Arrays.asList(superInterfaceNames));
        }
    }

    // 对一个Class进行预处理
    private boolean preHandle1Class(JavaClass javaClass) {
        String className = javaClass.getClassName();
        if (JavaCGClassMethodUtil.isObjectClass(className)) {
            logger.error("Object类所在jar包不需要添加到需要分析的jar包参数中，假如需要添加JDK中的类，可以解压相关的class文件到目录中并在 {} 中指定", JavaCGOtherConfigFileUseListEnum.OCFULE_JAR_DIR.getFileName());
            return false;
        }
        allClassNameSet.add(className);

        String[] interfaceNames = javaClass.getInterfaceNames();
        Method[] methods = javaClass.getMethods();

        if (interfaceNames.length > 0 &&
                methods != null && methods.length > 0 &&
                classImplementsMethodInfoMap.get(className) == null) {
            List<String> interfaceNameList = new ArrayList<>(interfaceNames.length);
            interfaceNameList.addAll(Arrays.asList(interfaceNames));

            // 记录类实现的接口，及类中可能涉及实现的相关方法
            List<MethodArgReturnTypes> implClassMethodWithArgTypesList = JavaCGByteCodeUtil.genImplClassMethodWithArgTypes(methods);
            classImplementsMethodInfoMap.put(className, new ClassImplementsMethodInfo(interfaceNameList, implClassMethodWithArgTypesList));

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
        } else if (JavaCGCommonNameConstants.CLASS_NAME_TRANSACTION_CALLBACK_WITHOUT_RESULT.equals(superClassName)) {
            // 找到TransactionCallbackWithoutResult实现类
            transactionCallbackWithoutResultChildClassMap.put(className, Boolean.FALSE);
        }

        // 若当前类的父类非Object，则记录当前类及对应的父类
        if (!JavaCGClassMethodUtil.isObjectClass(superClassName)) {
            classAndSuperMap.put(className, superClassName);
        }
        return true;
    }

    public void setClassImplementsMethodInfoMap(Map<String, ClassImplementsMethodInfo> classImplementsMethodInfoMap) {
        this.classImplementsMethodInfoMap = classImplementsMethodInfoMap;
    }

    public void setInterfaceMethodWithArgTypesMap(Map<String, List<MethodArgReturnTypes>> interfaceMethodWithArgTypesMap) {
        this.interfaceMethodWithArgTypesMap = interfaceMethodWithArgTypesMap;
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

    public void setAllClassNameSet(Set<String> allClassNameSet) {
        this.allClassNameSet = allClassNameSet;
    }

    public void setClassAndSuperMap(Map<String, String> classAndSuperMap) {
        this.classAndSuperMap = classAndSuperMap;
    }

    public void setInterfaceExtendsSet(Set<String> interfaceExtendsSet) {
        this.interfaceExtendsSet = interfaceExtendsSet;
    }

    public void setClassAndJarNum(ClassAndJarNum classAndJarNum) {
        this.classAndJarNum = classAndJarNum;
    }
}
