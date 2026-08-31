import my_crate.BatchObserver
import my_crate.PipelineResult
import org.rustlang.runtime.await

class LimitObserver(private val limit: Int) : BatchObserver {
    override fun accept(processed: Int): Boolean = processed <= limit
}

suspend fun main() {
    val prepared = my_crate.my_crate.process_batch(
        40,
        { value -> value + 2 },
        LimitObserver(100),
    )
    val outcome = my_crate.my_crate.confirm_batch(prepared)
        .await<PipelineResult>()

    when (outcome) {
        is PipelineResult.Success -> {
            val (count, elapsedMs) = outcome
            check(outcome.count == count)
            check(outcome.elapsed_ms == elapsedMs)
            println("completed: $count in ${elapsedMs}ms")
        }
        is PipelineResult.Rejected -> {
            val (code) = outcome
            println("rejected: $code")
        }
        else -> error("unknown PipelineResult implementation")
    }

    val rejected = my_crate.my_crate.confirm_batch(
        my_crate.my_crate.process_batch(40, { value -> value + 2 }, LimitObserver(10)),
    ).await<PipelineResult>()
    check(rejected is PipelineResult.Rejected)
    val (code) = rejected
    check(code == -1)
    check(rejected.value == code)
}
