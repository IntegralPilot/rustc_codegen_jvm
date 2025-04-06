package asmprocessor

import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassWriter
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    if (args.size != 2) {
        System.err.println("Usage: kotlin asmprocessor.ProcessorKt <input.class> <output.class>")
        exitProcess(1)
    }
    val inputFile = args[0]
    val outputFile = args[1]

    try {
        println("Reading: $inputFile")
        val originalBytecode: ByteArray = Files.readAllBytes(Paths.get(inputFile))

        val cr = ClassReader(originalBytecode)
        // Combine COMPUTE_FRAMES and COMPUTE_MAXS to generate new frames and adjust max values.
        val cw = ClassWriter(ClassWriter.COMPUTE_FRAMES or ClassWriter.COMPUTE_MAXS)

        // Use ClassReader.EXPAND_FRAMES to ensure full frame expansion before computing new frames.
        cr.accept(cw, ClassReader.EXPAND_FRAMES)

        val modifiedBytecode: ByteArray = cw.toByteArray()

        println("Writing processed bytecode to: $outputFile")
        Files.write(
            Paths.get(outputFile),
            modifiedBytecode,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING
        )

        println("Done.")

    } catch (e: Exception) {
        System.err.println("Error processing class file: ${e.message}")
        e.printStackTrace() // Print full stack trace for debugging
        exitProcess(1)
    }
}
