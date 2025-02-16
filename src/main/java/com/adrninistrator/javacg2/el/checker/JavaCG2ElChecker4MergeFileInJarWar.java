package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description:
 */
public class JavaCG2ElChecker4MergeFileInJarWar extends JavaCG2ElChecker {

    @Override
    protected void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig) {
        switch(elConfig){
            case ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR:
                elManager.checkIgnoreMergeFileInJarWar("a/b.jar");
                break;
            case ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR:
                elManager.checkIgnoreMergeFileInJarWar("a/b.class");
                break;
            case ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR:
                elManager.checkIgnoreMergeFileInJarWar("a/b.xml");
                break;
            default:
                break;
        }
    }
}
