package com.adrninistrator.javacg2.conf.enums;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CalleeRawActualEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/11/7
 * @description:
 */
public enum JavaCG2ConfigKeyEnum implements MainConfigInterface {
    CKE_PARSE_METHOD_CALL_TYPE_VALUE("parse.method.call.type.value",
            new String[]{"处理方法调用时是否解析被调用对象和参数可能的类型与值",
                    "开启后可支持识别多态、Spring Bean等使用的实际类型",
                    "例如对于方法调用 A a = new B(); a.func(123); 开启当前开关后可获得对象a类型为B，func方法调用时参数值为123"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_FIRST_PARSE_INIT_METHOD_TYPE("first.parse.init.method.type",
            new String[]{"处理类的方法前是否需要先解析构造函数以获取非静态字段可能的类型，仅当parse.method.call.type.value参数为true时才可以生效"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_ANALYSE_FIELD_RELATIONSHIP("analyse.field.relationship",
            new String[]{"是否需要分析dto的字段之间通过get/set方法的关联关系，仅当parse.method.call.type.value参数为true时才可以生效"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_CONTINUE_WHEN_ERROR("continue.when.error",
            new String[]{"解析方法出现异常时，是否要继续",
                    "若开启后在出现异常时不会抛出异常，会继续执行；若不开启则出现异常时会抛出异常终止处理"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_LOG_METHOD_SPEND_TIME("log.method.spend.time",
            new String[]{"是否在输出目录生成记录方法分析耗时的文件"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_OUTPUT_ROOT_PATH("output.root.path",
            new String[]{"生成文件的根目录，分隔符支持使用/或\\，末尾是否为分隔符不影响",
                    "生成解析结果文件时，根目录由该参数控制，默认生成到需要解析的jar文件所在目录",
                    "在解析jar文件时，若需要合并jar、war文件、目录合并为新的jar文件，所在目录由该参数控制，默认生成到需要解析的第一个文件、目录所在目录",
                    "假如需要解析jmod文件，则当前参数必须指定"},
            String.class, false, ""),
    CKE_OUTPUT_FILE_EXT("output.file.ext",
            new String[]{"指定生成文件后缀名，需要以“.”开头"},
            String.class, false, JavaCG2Constants.EXT_TXT),
    CKE_EL_DEBUG_MODE("el.debug.mode",
            new String[]{"是否开启表达式执行调试模式，若开启会在应用日志中输出表达式执行时的详细信息"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_HANDLE_CALLEE_NEW_RAW_ACTUAL("handle.callee.new.raw.actual",
            new String[]{"解析方法调用时，通过new创建的被调用类型使用原始类型还是实际类型的开关",
                    JavaCG2CalleeRawActualEnum.getAllInfo(),
                    "例如 Super1 obj = new Child1(); obj.func1(); ，被调用对象的原始类型为Super1，实际类型为Child1，通过该开关选择被调用对象使用的类型"
            },
            String.class, false, JavaCG2CalleeRawActualEnum.CRAE_ONLY_ACTUAL.getType()),
    CKE_HANDLE_CALLEE_SPRING_BEAN_RAW_ACTUAL("handle.callee.spring.bean.raw.actual",
            new String[]{"解析方法调用时，被调用对象为Spring Bean时（支持字段注入），类型使用原始类型还是实际类型的开关",
                    JavaCG2CalleeRawActualEnum.getAllInfo(),
                    "例如Spring Bean字段定义的类型为Super1，实际注入的类型为Child1，被调用对象的原始类型为Super1，实际类型为Child1，通过该开关选择被调用对象使用的类型"
            },
            String.class, false, JavaCG2CalleeRawActualEnum.CRAE_ONLY_ACTUAL.getType()),
    CKE_MERGE_SEPARATE_FAT_JAR("merge.separate.fat.jar",
            new String[]{"在合并需要解析的jar文件时，是否合并出一个单独的fat jar。仅包含.class文件，且所有的jar文件都合并到从根目录开始"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_PARSE_JAR_COMPATIBILITY_MODE("parse.jar.compatibility.mode",
            new String[]{"是否使用Jar兼容性检查模式，仅解析类、方法、字段、类的注解等基础信息"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_PARSE_ONLY_CLASS_MODE("parse.only.class.mode",
            new String[]{"是否仅解析类及类的注解信息，比 " + CKE_PARSE_JAR_COMPATIBILITY_MODE.getKey() + " 优先级高"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_JMOD_PROGRAM_PATH("jmod.program.path",
            new String[]{"指定用于解析 .jmod 文件的jmod程序完整路径",
                    "假如指定的 .jmod 文件在JDK的jmods目录中，可找到bin目录的jmod程序，则当前参数可省略",
                    "假如无法找到JDK bin目录的jmod程序，则当前参数不能省略"
            },
            String.class, false, ""),
    CKE_JDK_RUNTIME_MAJOR_VERSION("jdk.runtime.major.version",
            new String[]{"解析的 jar 文件在运行时使用的 JDK 主版本号",
                    "例如 8、11、17、21 等",
                    "默认为 8 ，代表 JDK8 及以下版本"
            },
            Integer.class, false, "8"),
    ;

    // 参数key
    private final String key;
    // 参数描述
    private final String[] descriptions;
    // 参数类型
    private final Class<?> type;
    // 是否不允许为空
    private final boolean notBlank;
    // 默认值
    private final String defaultValue;

    JavaCG2ConfigKeyEnum(String key, String[] descriptions, Class<?> type, boolean notBlank, String defaultValue) {
        this.key = key;
        this.descriptions = descriptions;
        this.type = type;
        this.notBlank = notBlank;
        this.defaultValue = defaultValue;
    }

    @Override
    public String getEnumConstantName() {
        return name();
    }

    @Override
    public String getKey() {
        return key;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String getConfigPrintInfo() {
        return key + " " + JavaCG2ConfigKeyEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    @Override
    public boolean isNotBlank() {
        return notBlank;
    }

    @Override
    public String getDefaultValue() {
        return defaultValue;
    }

    @Override
    public String toString() {
        return key;
    }

    @Override
    public String getFileName() {
        return JavaCG2DirEnum.IDE_CONFIG.getDirName() + "/config.properties";
    }
}
