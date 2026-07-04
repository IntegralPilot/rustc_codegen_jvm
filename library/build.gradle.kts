plugins {
    java
}

group = "org.rustlang"
version = "0.1.0"

repositories {
    mavenCentral()
}

tasks.withType<JavaCompile>().configureEach {
    options.release.set(8)
    options.compilerArgs.add("-Xlint:-options")
}
