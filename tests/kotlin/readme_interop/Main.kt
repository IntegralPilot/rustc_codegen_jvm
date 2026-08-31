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
        is PipelineResult.Success -> println("completed: ${outcome.field0}")
        is PipelineResult.Rejected -> println("rejected: ${outcome.field0}")
        else -> error("unknown PipelineResult implementation")
    }

    val rejected = my_crate.my_crate.confirm_batch(
        my_crate.my_crate.process_batch(40, { value -> value + 2 }, LimitObserver(10)),
    ).await<PipelineResult>()
    check(rejected is PipelineResult.Rejected)
    check(rejected.field0 == -1)
}
