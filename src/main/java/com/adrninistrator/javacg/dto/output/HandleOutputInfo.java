package com.adrninistrator.javacg.dto.output;

/**
 * @author adrninistrator
 * @date 2022/11/5
 * @description: java-callgraph2处理结果信息
 */
public class HandleOutputInfo {

    // 保存当前的输出目录路径
    private String outputDirPath;

    // 保存当前的输出文件路径-jar包信息
    private String jarInfoOutputFilePath;

    // 保存当前的输出文件路径-引用的类
    private String classNameOutputFilePath;

    // 保存当前的输出文件路径-方法调用
    private String methodCallOutputFilePath;

    // 保存当前的输出文件路径-lambda表达式方法信息
    private String lambdaMethodInfoOutputFilePath;

    // 保存当前输出的文件路径-类的注解
    private String classAnnotationOutputFilePath;

    // 保存当前输出的文件路径-方法的注解
    private String methodAnnotationOutputFilePath;

    // 保存当前输出的文件路径-方法代码行号
    private String methodLineNumberOutputFilePath;

    // 保存当前输出的文件路径-方法调用的信息
    private String methodCallInfoOutputFilePath;

    // 保存当前输出的文件路径-类的信息
    private String classInfoOutputFilePath;

    // 保存当前输出的文件路径-方法的信息
    private String methodInfoOutputFilePath;

    // 保存当前输出的文件路径-继承与实现相关信息
    private String extendsImplOutputFilePath;

    // 保存当前输出的文件路径-Spring Bean信息
    private String springBeanOutputFilePath;

    // 保存当前输出的文件路径-继承父类与实现接口时的签名中的类名信息
    private String classSignatureEI1OutputFilePath;

    //
    public String getOutputDirPath() {
        return outputDirPath;
    }

    public void setOutputDirPath(String outputDirPath) {
        this.outputDirPath = outputDirPath;
    }

    public String getJarInfoOutputFilePath() {
        return jarInfoOutputFilePath;
    }

    public void setJarInfoOutputFilePath(String jarInfoOutputFilePath) {
        this.jarInfoOutputFilePath = jarInfoOutputFilePath;
    }

    public String getClassNameOutputFilePath() {
        return classNameOutputFilePath;
    }

    public void setClassNameOutputFilePath(String classNameOutputFilePath) {
        this.classNameOutputFilePath = classNameOutputFilePath;
    }

    public String getMethodCallOutputFilePath() {
        return methodCallOutputFilePath;
    }

    public void setMethodCallOutputFilePath(String methodCallOutputFilePath) {
        this.methodCallOutputFilePath = methodCallOutputFilePath;
    }

    public String getLambdaMethodInfoOutputFilePath() {
        return lambdaMethodInfoOutputFilePath;
    }

    public void setLambdaMethodInfoOutputFilePath(String lambdaMethodInfoOutputFilePath) {
        this.lambdaMethodInfoOutputFilePath = lambdaMethodInfoOutputFilePath;
    }

    public String getClassAnnotationOutputFilePath() {
        return classAnnotationOutputFilePath;
    }

    public void setClassAnnotationOutputFilePath(String classAnnotationOutputFilePath) {
        this.classAnnotationOutputFilePath = classAnnotationOutputFilePath;
    }

    public String getMethodAnnotationOutputFilePath() {
        return methodAnnotationOutputFilePath;
    }

    public void setMethodAnnotationOutputFilePath(String methodAnnotationOutputFilePath) {
        this.methodAnnotationOutputFilePath = methodAnnotationOutputFilePath;
    }

    public String getMethodLineNumberOutputFilePath() {
        return methodLineNumberOutputFilePath;
    }

    public void setMethodLineNumberOutputFilePath(String methodLineNumberOutputFilePath) {
        this.methodLineNumberOutputFilePath = methodLineNumberOutputFilePath;
    }

    public String getMethodCallInfoOutputFilePath() {
        return methodCallInfoOutputFilePath;
    }

    public void setMethodCallInfoOutputFilePath(String methodCallInfoOutputFilePath) {
        this.methodCallInfoOutputFilePath = methodCallInfoOutputFilePath;
    }

    public String getClassInfoOutputFilePath() {
        return classInfoOutputFilePath;
    }

    public void setClassInfoOutputFilePath(String classInfoOutputFilePath) {
        this.classInfoOutputFilePath = classInfoOutputFilePath;
    }

    public String getMethodInfoOutputFilePath() {
        return methodInfoOutputFilePath;
    }

    public void setMethodInfoOutputFilePath(String methodInfoOutputFilePath) {
        this.methodInfoOutputFilePath = methodInfoOutputFilePath;
    }

    public String getExtendsImplOutputFilePath() {
        return extendsImplOutputFilePath;
    }

    public void setExtendsImplOutputFilePath(String extendsImplOutputFilePath) {
        this.extendsImplOutputFilePath = extendsImplOutputFilePath;
    }

    public String getSpringBeanOutputFilePath() {
        return springBeanOutputFilePath;
    }

    public void setSpringBeanOutputFilePath(String springBeanOutputFilePath) {
        this.springBeanOutputFilePath = springBeanOutputFilePath;
    }

    public String getClassSignatureEI1OutputFilePath() {
        return classSignatureEI1OutputFilePath;
    }

    public void setClassSignatureEI1OutputFilePath(String classSignatureEI1OutputFilePath) {
        this.classSignatureEI1OutputFilePath = classSignatureEI1OutputFilePath;
    }
}
