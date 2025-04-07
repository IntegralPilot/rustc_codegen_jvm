plugins {
    kotlin("jvm") version "2.1.20"
    application
}

group = "org.rustlang"
version = "0.1.0"

repositories {
    mavenCentral()
}

kotlin {
    jvmToolchain(21) // latest LTS. note to self: update when they release a new LTS.
}