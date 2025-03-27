package com.adrninistrator.javacg2.el.checker;

import com.adrninistrator.javacg2.el.enums.JavaCG2ElConfigEnum;
import com.adrninistrator.javacg2.el.manager.JavaCG2ElManager;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/2/13
 * @description:
 */
public class JavaCG2ElChecker4MergeFileInJarWar extends JavaCG2ElChecker {

    @Override
    protected void javaCG2DoCheck(JavaCG2ElManager elManager, JavaCG2ElConfigEnum elConfig) {
        switch (elConfig) {
            case ECE_MERGE_FILE_IGNORE_JAR_IN_JAR_WAR:
                elManager.checkIgnoreMergeFileInJarWar("a/b.jar");
                break;
            case ECE_MERGE_FILE_IGNORE_CLASS_IN_JAR_WAR:
                elManager.checkIgnoreMergeFileInJarWar("a/b.class");
                break;
            case ECE_MERGE_FILE_IGNORE_OTHER_IN_JAR_WAR:
                elManager.checkIgnoreMergeFileInJarWar("a/b.xml");
                break;
            case ECE_MERGE_FILE_IGNORE_JAR_WAR_BY_CLASS_DIR_PREFIX:
                Map<Integer, Set<String>> classDirPrefixMap = new HashMap<>();
                classDirPrefixMap.put(1, JavaCG2Util.genSetFromArray("a1", "a2", "a3"));
                classDirPrefixMap.put(2, JavaCG2Util.genSetFromArray("a1.b1", "a2.b2", "a3.b3"));
                classDirPrefixMap.put(3, JavaCG2Util.genSetFromArray("a1.b1.c1", "a2.b2.c2", "a3.b3.c3"));
                elManager.checkIgnoreJarWarByClassDirPrefix(classDirPrefixMap);
                break;
            default:
                break;
        }
    }
}
