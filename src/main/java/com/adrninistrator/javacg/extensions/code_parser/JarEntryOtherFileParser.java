package com.adrninistrator.javacg.extensions.code_parser;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description: 对jar包中的其他文件解析类的接口
 */
public interface JarEntryOtherFileParser extends CodeParserInterface {

    /**
     * 返回需要处理jar包中的其他文件的后缀数组（除.class文件外），忽略大小写，不需要以"."开头，例如"xml"
     * 若需要使以下 parseJarEntryOtherFile() 方法被调用，则当前方法需要返回对应的文件后缀数组
     */
    String[] chooseJarEntryOtherFileExt();

    /**
     * 处理jar包中的文件（除class文件外）
     * 仅当以上 chooseJarEntryOtherFileExt() 方法返回的数组中包含当前文件后缀时，才会调用当前方法
     *
     * @param inputStream  当前文件对应的输入流
     * @param jarEntryName 当前文件在jar包中的名称
     */
    void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName);
}
