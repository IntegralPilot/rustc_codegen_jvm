package stackmapadder

import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassWriter
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import kotlin.io.path.absolute
import kotlin.io.path.exists
import kotlin.io.path.isRegularFile
import kotlin.io.path.name
import kotlin.system.exitProcess

/**
 * Reads an input class file, computes stack map frames using ASM,
 * and writes the result to an output class file.
 */
fun processWithAsm(inputBytes: ByteArray): ByteArray {
    // ClassReader reads the bytecode
    val cr = ClassReader(inputBytes)

    // ClassWriter computes frames and max stack/locals automatically
    // EXPAND_FRAMES is needed for COMPUTE_FRAMES when input might have compressed frames (Java 6+)
    val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)

    // Start the processing chain
    // ClassReader.EXPAND_FRAMES is crucial here for ASM to correctly process existing frames
    // before recomputing them.
    cr.accept(cw, ClassReader.EXPAND_FRAMES)

    println("Info: Successfully processed class '${cr.className.replace('/', '.')}' with ASM.")
    return cw.toByteArray()
}

fun main(args: Array<String>) {
    if (args.size != 2) {
        System.err.println("Usage: java -jar stackmap-adder.jar <input.class> <output.class>")
        System.err.println("   Example: gradle run --args=\"Input.class OutputWithFrames.class\"")
        exitProcess(1)
    }

    val inputFile = Paths.get(args[0]).absolute()
    val outputFile = Paths.get(args[1]).absolute()

    // --- Input Validation ---
    if (!inputFile.exists()) {
        System.err.println("Error: Input file not found: $inputFile")
        exitProcess(1)
    }
    if (!inputFile.isRegularFile() || !inputFile.name.endsWith(".class", ignoreCase = true)) {
        System.err.println("Error: Input file must be a .class file: $inputFile")
        exitProcess(1)
    }
    if (inputFile == outputFile) {
         System.err.println("Error: Input and output file paths cannot be the same.")
         exitProcess(1)
    }

    // --- Processing ---
    try {
        println("Reading input class file: $inputFile")
        val originalBytecode: ByteArray = Files.readAllBytes(inputFile)

        println("Adding/Computing StackMapFrames using ASM...")
        val modifiedBytecode = processWithAsm(originalBytecode)

        println("Writing modified class file to: $outputFile")
        outputFile.parent?.let { Files.createDirectories(it) } // Create output directory if needed
        Files.write(outputFile, modifiedBytecode, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

        println("Done.")

    } catch (e: Exception) {
        System.err.println("An error occurred during processing:")
        e.printStackTrace()
        // Attempt to clean up potentially partially written output file
        try {
            Files.deleteIfExists(outputFile)
        } catch (_: Exception) {
            // Ignore cleanup error
        }
        exitProcess(1)
    }
}