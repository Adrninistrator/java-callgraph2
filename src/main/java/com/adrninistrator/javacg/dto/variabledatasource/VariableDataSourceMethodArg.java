package com.adrninistrator.javacg.dto.variabledatasource;

/**
 * @author adrninistrator
 * @date 2023/6/12
 * @description: 变量的数据来源，使用方法参数
 */
public class VariableDataSourceMethodArg extends AbstractVariableDataSource {

    // 参数的序号（从1开始）
    private final int argSeq;

    // 参数的类型
    private final String argType;

    public VariableDataSourceMethodArg(int argSeq, String argType) {
        this.argSeq = argSeq;
        this.argType = argType;
    }

    /**
     * 比较与另一个对象是否相同
     *
     * @param added
     * @return false: 不相同 true: 相同
     */
    public boolean compare(VariableDataSourceMethodArg added) {
        return this.argSeq == added.argSeq &&
                this.argType.equals(added.argType);
    }

    public int getArgSeq() {
        return argSeq;
    }

    public String getArgType() {
        return argType;
    }

    @Override
    public String toString() {
        return "VariableDataSourceMethodArg{" +
                "argSeq=" + argSeq +
                ", argType='" + argType + '\'' +
                '}';
    }
}
