plugins {
    kotlin("jvm") version "2.1.20"
    id("com.gradleup.shadow") version "8.3.6"
    application
}

group = "com.integralpilot"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    google()
}

dependencies {
    implementation("com.android.tools:r8:8.9.35")
}

application {
    mainClass.set("optimise2.ProcessorKt")
}


kotlin {
    jvmToolchain(21) // latest LTS. note to self: update when they release a new LTS.
}