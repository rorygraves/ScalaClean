package objectannotation;

@interface UsedJavaAnnotation1 {
    Class clazz();
}
@interface UsedJavaAnnotation2 {
    Class[] clazz();
}
@interface UsedJavaAnnotation3 {
    UsedJavaAnnotation1 ref();
}
