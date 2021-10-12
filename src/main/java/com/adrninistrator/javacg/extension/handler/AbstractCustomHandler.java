package com.adrninistrator.javacg.extension.handler;

import com.adrninistrator.javacg.extension.dto.CustomData;
import com.adrninistrator.javacg.extension.interfaces.CustomHandlerInterface;

import java.util.List;

/**
 * @author Adrninistrator
 * @date 2021/8/10
 * @description:
 */
public abstract class AbstractCustomHandler implements CustomHandlerInterface {

    // 保存自定义数据的列表
    protected List<CustomData> customDataList;

    @Override
    public List<CustomData> getCustomDataList() {
        return customDataList;
    }
}
