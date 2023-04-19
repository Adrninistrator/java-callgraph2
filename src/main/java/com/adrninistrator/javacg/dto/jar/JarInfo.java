package com.adrninistrator.javacg.dto.jar;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description: jar包信息
 */
public class JarInfo {
    private final int jarNum;

    private final String jarType;

    private final String jarPath;

    public JarInfo(int jarNum, String jarType, String jarPath) {
        this.jarNum = jarNum;
        this.jarType = jarType;
        this.jarPath = jarPath;
    }

    //
    public int getJarNum() {
        return jarNum;
    }

    public String getJarType() {
        return jarType;
    }

    public String getJarPath() {
        return jarPath;
    }
}
