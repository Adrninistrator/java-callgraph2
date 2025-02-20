package com.adrninistrator.javacg2.el.manager;

import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.el.checker.ElChecker;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.el.handler.ElHandler;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/2
 * @description: 表达式管理类基类
 */
public abstract class ElManager {

    private static final Logger logger = LoggerFactory.getLogger(ElManager.class);
    /*
            保存表达式处理类的Map
            key     配置文件名称
            value   表达式处理类
         */
    private final Map<String, ElHandler> elHandlerMap = new HashMap<>();

    protected ElManager(BaseConfigureWrapper configureWrapper, ElConfigInterface[] elConfigInterfaces) {
        // 生成ElHandler对象
        for (ElConfigInterface elConfig : elConfigInterfaces) {
            Class<? extends ElChecker> elCheckerClass = elConfig.getElCheckClass();
            if (elCheckerClass == null) {
                continue;
            }
            String fileName = elConfig.getKey();
            ElHandler elHandler = configureWrapper.genElHandler(elConfig);
            elHandlerMap.put(fileName, elHandler);

            // 检查表达式
            try {
                ElChecker elChecker = elCheckerClass.newInstance();
                elChecker.check(this, elConfig);
            } catch (Exception e) {
                logger.error("创建对象实例失败 {}", elCheckerClass.getName());
                throw new JavaCG2RuntimeException("创建对象实例失败");
            }
        }
    }

    public ElHandler getElHandlerMap(ElConfigInterface elConfig) {
        ElHandler elHandler = elHandlerMap.get(elConfig.getKey());
        if (elHandler == null) {
            throw new JavaCG2RuntimeException("未找到对应的表达式处理类 " + elConfig.getKey());
        }
        return elHandler;
    }
}
