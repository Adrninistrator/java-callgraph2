package com.adrninistrator.javacg.dto.jar;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class JarInfo {

    private static int staticJarNum = 0;

    private int jarNum;

    private String jarType;

    private String jarPath;

    public static JarInfo genJarInfo(String jarType, String jarPath) {
        JarInfo jarInfo = new JarInfo();
        jarInfo.setJarNum(++staticJarNum);
        jarInfo.setJarType(jarType);
        jarInfo.setJarPath(jarPath);

        return jarInfo;
    }

    //
    public int getJarNum() {
        return jarNum;
    }

    public void setJarNum(int jarNum) {
        this.jarNum = jarNum;
    }

    public String getJarType() {
        return jarType;
    }

    public void setJarType(String jarType) {
        this.jarType = jarType;
    }

    public String getJarPath() {
        return jarPath;
    }

    public void setJarPath(String jarPath) {
        this.jarPath = jarPath;
    }
}
