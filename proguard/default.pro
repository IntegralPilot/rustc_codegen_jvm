-keep public class * { public static void main(java.lang.String[]); }
-keep class * {
    <fields>;
}
-keepclassmembers class * implements * {
    <methods>;
}

-keepattributes MethodParameters