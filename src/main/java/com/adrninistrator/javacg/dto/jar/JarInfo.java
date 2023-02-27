package com.adrninistrator.javacg.dto.jar;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class JarInfo {
    private static int staticJarNum = 0;

    private final int jarNum;

    private final String jarType;

    private final String jarPath;

    public JarInfo(String jarType, String jarPath) {
        this.jarNum = ++staticJarNum;
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
