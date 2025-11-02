-keep public class *
-keep class * {
    <fields>;
}
-keepclassmembers class * implements * {
    <methods>;
}

-keepattributes MethodParameters
-keepattributes InnerClasses
-keepattributes EnclosingMethod