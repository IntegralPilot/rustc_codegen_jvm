plugins {
    kotlin("jvm") version "2.1.20"
    application
}

group = "org.rustlang"
version = "0.1.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib"))
}

kotlin {
    jvmToolchain(21)
}