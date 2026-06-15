plugins {
    java
    application
}

group = "org.rustlang"
version = "0.1.0"

repositories {
    mavenCentral()
}

application {
    // The shim jar is consumed as a library, not run; this only satisfies the application
    // plugin's start-script generation (which preserves the build/distributions/.../lib/ layout
    // that build.py reads). The class is never executed.
    mainClass.set("org.rustlang.core.Core")
}

tasks.withType<JavaCompile> {
    options.release.set(8)
}
