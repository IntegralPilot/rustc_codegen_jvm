package optimise2

// R8 imports
import com.android.tools.r8.R8
import com.android.tools.r8.R8Command
import com.android.tools.r8.OutputMode
import com.android.tools.r8.origin.Origin
import com.android.tools.r8.DiagnosticsHandler // Default handler is usually fine
import com.android.tools.r8.CompilationFailedException
import com.android.tools.r8.ProgramResourceProvider // Can be useful for more complex inputs
import com.android.tools.r8.ClassFileConsumer // Used with OutputMode.ClassFile

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import kotlin.io.path.*
import kotlin.system.exitProcess
// java.util.Properties might not be needed anymore unless used elsewhere
// java.io.Closeable is generally good practice but less critical here now

// findJavaRuntimeLibs function remains the same...
fun findJavaRuntimeLibs(): List<Path> { // Return List<Path> for consistency with R8 API
    val javaHome = System.getProperty("java.home")
    val jmodsPath = Paths.get(javaHome, "jmods")
    val rtJarPath = Paths.get(javaHome, "lib", "rt.jar") // For older Java versions

    return when {
        Files.isDirectory(jmodsPath) -> {
            println("Info: Found Java runtime jmods directory: $jmodsPath")
            listOf(jmodsPath)
            // R8 handles the jmods directory directly
        }
        Files.exists(rtJarPath) -> {
             println("Info: Found Java runtime rt.jar: $rtJarPath")
             listOf(rtJarPath)
        }
        else -> {
            println("Warning: Could not reliably determine Java runtime library path from '$javaHome'. R8 analysis might be incomplete. Consider specifying libraries manually in the R8/ProGuard config.")
            emptyList()
        }
    }
}


/**
 * Runs R8 programmatically based on a configuration file.
 *
 * @param inputJar The input JAR file to process.
 * @param outputJar The path where the processed JAR file will be written.
 * @param configFile The ProGuard/R8 configuration file (.pro).
 * @param javaRuntimeLibPaths List of Java runtime library paths (e.g., rt.jar path or jmods dir path).
 * @return True if R8 execution was successful, false otherwise.
 */
fun runR8Programmatically(
    inputJar: Path,
    outputJar: Path,
    configFile: Path,
    javaRuntimeLibPaths: List<Path>
): Boolean {
    println("Configuring R8 from file: $configFile")
    // R8 needs the output directory to exist
    outputJar.parent?.createDirectories()

    // Delete potential leftover mapping/seeds files before running
    // R8 uses the paths specified in the config file directly.
    // We don't have direct access to parsed config values like `-printmapping` here
    // before running, so we rely on the user potentially clearing them manually
    // or using standard names if needed before invocation. A better approach
    // might parse the config file manually first to find these paths if needed.
    // For simplicity, we assume R8 handles overwriting or the user manages cleanup.
    // Example cleanup (if you know the typical names):
    // Files.deleteIfExists(Paths.get("mapping.txt")) // Common R8 map file name
    // Files.deleteIfExists(Paths.get("seeds.txt"))   // Common R8 seeds file name

    try {
        // 1. Build R8 Command
        val commandBuilder = R8Command.builder()

        // Use default diagnostics handler (prints to stderr)
        // val diagnosticsHandler = DiagnosticsHandler { ... } // Custom handler
        // commandBuilder.setDiagnosticsHandler(diagnosticsHandler)

        // 2. Set Input Program File(s)
        commandBuilder.addProgramFiles(inputJar)
        println("Input JAR set to: ${inputJar.absolutePathString()}")

        // 3. Set Output Path and Mode (ClassFile for JAR output)
        commandBuilder.setOutput(outputJar, OutputMode.ClassFile)
        println("Output JAR set to: ${outputJar.absolutePathString()}")

        // 4. Set Library Files (Including Java Runtime)
        javaRuntimeLibPaths.forEach { libPath ->
            if (Files.exists(libPath)) {
                commandBuilder.addLibraryFiles(libPath)
                println("Adding Library File/Dir: ${libPath.absolutePathString()}")
            } else {
                 println("Warning: Library file/dir does not exist: ${libPath.absolutePathString()}")
            }
        }
        // Note: R8 will also respect -libraryjars from the config file. If the same
        // library is specified both here and in the config, it shouldn't cause issues.
        if (javaRuntimeLibPaths.isEmpty()) {
            println("Warning: No Java runtime libraries automatically detected or added programmatically. Ensure they are specified via '-libraryjars' in $configFile for correct analysis.")
        }

        // 5. Add ProGuard Configuration File(s)
        // R8 parses ProGuard configuration files directly.
        commandBuilder.addProguardConfigurationFiles(configFile)
        println("Using ProGuard configuration rules from: $configFile")

        // 6. Set Compilation Mode (RELEASE enables shrinking, optimization, obfuscation)
        // Other modes: DEBUG (minimal processing)
        commandBuilder.setMode(com.android.tools.r8.CompilationMode.RELEASE)


        // 7. Optional: Set other R8 specific options if needed
        // commandBuilder.setDisableMinification(true) // If you only want shrinking
        // commandBuilder.setDisableTreeShaking(true) // If you only want optimization/obfuscation
        // commandBuilder.setDisableDesugaring(true) // Likely desired for JAR->JAR unless targeting older Java VMs

        // R8 specific API options for mapping/seeds (alternative to config file rules)
        // commandBuilder.setProguardMapOutputPath(Paths.get("your-map-file.txt"))
        // commandBuilder.setProguardSeedsOutputPath(Paths.get("your-seeds.txt"))


        // 8. Execute R8
        println("Running R8 engine...")
        R8.run(commandBuilder.build()) // Can throw CompilationFailedException
        println("R8 execution finished.")

        if (!Files.exists(outputJar)) {
             System.err.println("Error: R8 finished, but the output JAR file was not created: $outputJar")
             // R8 might have failed silently or the output path was wrong.
             // R8Exception during run() usually indicates failure.
             return false
        }

        return true

    } catch (e: CompilationFailedException) {
        System.err.println("Error: R8 compilation failed: ${e.message ?: "No specific message."}")
        // R8's default diagnostics handler usually prints detailed errors to stderr already.
        // e.getCause() might provide more details if needed.
        return false
    } catch (e: Exception) {
        System.err.println("Error during R8 configuration or execution: ${e.message}")
        e.printStackTrace()
        return false
    }
}

fun main(args: Array<String>) {
    if (args.size != 3) {
        System.err.println("Usage: java -jar r8-optimizer.jar <input.jar> <output.jar> <proguard-config.pro>")
        System.err.println("   Example: gradle run --args=\"input.jar output.jar myconfig.pro\"")
        exitProcess(1)
    }

    val inputJar = Paths.get(args[0]).absolute()
    val outputJar = Paths.get(args[1]).absolute()
    val configFile = Paths.get(args[2]).absolute()

    // --- Input Validation (remains mostly the same) ---
    if (!inputJar.exists()) {
        System.err.println("Error: Input JAR not found: $inputJar")
        exitProcess(1)
    }
    if (!inputJar.isRegularFile() || !inputJar.name.endsWith(".jar", ignoreCase = true)) {
        System.err.println("Error: Input file must be a .jar file: $inputJar")
        exitProcess(1)
    }
    if (!configFile.exists()) {
        System.err.println("Error: R8/ProGuard configuration file not found: $configFile")
        exitProcess(1)
    }
     if (!configFile.isRegularFile()) {
         System.err.println("Error: R8/ProGuard configuration path must be a regular file: $configFile")
         exitProcess(1)
    }
     if (inputJar == outputJar) {
         System.err.println("Error: Input and output JAR paths cannot be the same.")
         exitProcess(1)
     }

    // --- Processing ---
    // Define potential output paths based on standard conventions or parsing config
    // val mapFilePath = Paths.get("mapping.txt") // Common R8 map file name
    // val seedsFilePath = Paths.get("seeds.txt") // Common R8 seeds file name

    try {
        println("Optimizing JAR with R8: $inputJar")
        println("Outputting to: $outputJar")
        println("Using configuration: $configFile")

        // 1. Find Java Runtime Libraries
        val javaRuntimeLibPaths = findJavaRuntimeLibs() // Now returns List<Path>

        // 2. Run R8
        val r8Success = runR8Programmatically(
            inputJar,
            outputJar,
            configFile,
            javaRuntimeLibPaths
        )

        if (!r8Success) {
            System.err.println("Error: R8 processing failed.")
            // R8 might have created partial output, attempt cleanup
            Files.deleteIfExists(outputJar)
            // Optional: Clean up map/seeds if paths are known
            // Files.deleteIfExists(mapFilePath)
            // Files.deleteIfExists(seedsFilePath)
            exitProcess(1)
        }

        println("Successfully optimized JAR stored at: $outputJar")
        // Optional: Check if map/seeds files were generated based on config

        println("Done.")

    } catch (e: Exception) {
        // Catch any unexpected errors from main logic
        System.err.println("An unexpected error occurred: ${e.message}")
        e.printStackTrace()
        // Attempt cleanup
        Files.deleteIfExists(outputJar)
        // Files.deleteIfExists(mapFilePath)
        // Files.deleteIfExists(seedsFilePath)
        exitProcess(1)
    }
}