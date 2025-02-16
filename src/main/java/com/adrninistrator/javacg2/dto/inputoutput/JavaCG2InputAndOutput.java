package com.adrninistrator.javacg2.dto.inputoutput;

import com.adrninistrator.javacg2.conf.JavaCG2ConfInfo;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.dto.output.JavaCG2OtherRunResult;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 保存需要传递的输入与输出数据
 */
public class JavaCG2InputAndOutput {

    private JavaCG2ConfInfo javaCG2ConfInfo;

    private JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    private JavaCG2ElManager javaCG2ElManager;

    private JavaCG2OutputInfo javaCG2OutputInfo;

    private JavaCG2OtherRunResult javaCG2OtherRunResult;

    public JavaCG2ConfInfo getJavaCG2ConfInfo() {
        return javaCG2ConfInfo;
    }

    public void setJavaCG2ConfInfo(JavaCG2ConfInfo javaCG2ConfInfo) {
        this.javaCG2ConfInfo = javaCG2ConfInfo;
    }

    public JavaCG2ConfigureWrapper getJavaCG2ConfigureWrapper() {
        return javaCG2ConfigureWrapper;
    }

    public void setJavaCG2ConfigureWrapper(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper) {
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
    }

    public JavaCG2ElManager getJavaCG2ElManager() {
        return javaCG2ElManager;
    }

    public void setJavaCG2ElManager(JavaCG2ElManager javaCG2ElManager) {
        this.javaCG2ElManager = javaCG2ElManager;
    }

    public JavaCG2OutputInfo getJavaCG2OutputInfo() {
        return javaCG2OutputInfo;
    }

    public void setJavaCG2OutputInfo(JavaCG2OutputInfo javaCG2OutputInfo) {
        this.javaCG2OutputInfo = javaCG2OutputInfo;
    }

    public JavaCG2OtherRunResult getJavaCG2OtherRunResult() {
        return javaCG2OtherRunResult;
    }

    public void setJavaCG2OtherRunResult(JavaCG2OtherRunResult javaCG2OtherRunResult) {
        this.javaCG2OtherRunResult = javaCG2OtherRunResult;
    }
}
