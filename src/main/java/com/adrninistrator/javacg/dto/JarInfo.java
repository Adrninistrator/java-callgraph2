package com.adrninistrator.javacg.dto;

/**
 * @author adrninistrator
 * @date 2022/2/13
 * @description:
 */
public class JarInfo {

    private int jarNum;

    private String jarType;

    private String jarPath;

    public static JarInfo genJarInfo(int jarNum, String jarType, String jarPath) {
        JarInfo jarInfo = new JarInfo();
        jarInfo.setJarNum(jarNum);
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
