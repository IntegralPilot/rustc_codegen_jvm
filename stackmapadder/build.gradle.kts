plugins {
    kotlin("jvm") version "2.1.20"
    id("com.gradleup.shadow") version "8.3.6"
    application
}

group = "com.integralpilot"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.ow2.asm:asm:9.8")

    testImplementation(kotlin("test"))
}

application {
    mainClass.set("stackmapadder.ProcessorKt")
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21) // latest LTS. note to self: update when they release a new LTS.
}