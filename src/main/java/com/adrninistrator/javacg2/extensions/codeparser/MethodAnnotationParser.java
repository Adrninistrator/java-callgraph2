package com.adrninistrator.javacg2.extensions.codeparser;

import com.adrninistrator.javacg2.dto.call.MethodCallList;
import org.apache.bcel.classfile.AnnotationEntry;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description: 对方法注解解析类的接口
 */
public interface MethodAnnotationParser extends CodeParserInterface {

    /**
     * 返回需要处理方法注解的注解类名
     * 若需要使以下 parseMethodAnnotation() 方法被调用，则当前方法需要返回对应的方法注解的注解类名
     */
    String[] chooseMethodAnnotationClassName();

    /**
     * 处理方法注解
     * 仅当以上 chooseMethodAnnotationClassName() 方法返回的数组中包含当前方法注解的注解类名时，才会调用当前方法
     *
     * @param callerClassName      当前方法的类名
     * @param callerMethodName     当前方法的方法名
     * @param callerMethodArgTypes 当前方法的参数类型
     * @param callerReturnType     当前方法的返回类型
     * @param annotationClassName  当前处理的注解类名
     * @param annotationEntry      当前处理的注解信息
     * @param methodCallList       方法调用信息列表
     */
    void parseMethodAnnotation(String callerClassName,
                               String callerMethodName,
                               String callerMethodArgTypes,
                               String callerReturnType,
                               String annotationClassName,
                               AnnotationEntry annotationEntry,
                               MethodCallList methodCallList);
}
