package com.adrninistrator.javacg.extensions.code_parser;

import com.adrninistrator.javacg.extensions.dto.ExtendedData;

import java.util.List;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description:
 */
public abstract class AbstractCustomCodeParser implements CustomCodeParserInterface {

    // 保存自定义数据的列表
    protected List<ExtendedData> extendedDataList;

    @Override
    public List<ExtendedData> getExtendedDataList() {
        return extendedDataList;
    }
}
