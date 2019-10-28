package scalaclean.test.annotation;

@interface BasicJava{};
@interface SingleJava{
    int singleValue();
};

@interface ComplexJava{
    int intValue();
    double doubleValue();
    String stringValue();
    boolean booleanValue();

    int intValueWithDefault() default 42;
}
