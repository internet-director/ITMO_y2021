plugins {
    kotlin("jvm") version "1.9.10"
    application
}

group = "ru.itmo.mpp"

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

sourceSets.main {
    java.srcDir("src")
}

application {
    mainClass.set("VerifierKt")
}

tasks["build"].dependsOn("run")