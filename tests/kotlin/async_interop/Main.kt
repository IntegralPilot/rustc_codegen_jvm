import async_interop.AsyncOutcome
import org.rustlang.runtime.await
import org.rustlang.runtime.KotlinFutureInterop
import org.rustlang.runtime.RustFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executor
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import kotlin.coroutines.AbstractCoroutineContextElement
import kotlin.coroutines.Continuation
import kotlin.coroutines.ContinuationInterceptor
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.startCoroutine

private const val DISPATCHER_THREAD = "kotlin-rust-async-dispatcher"

private class ExecutorDispatcher(
    private val executor: Executor,
) : AbstractCoroutineContextElement(ContinuationInterceptor), ContinuationInterceptor {
    val dispatchCount = AtomicInteger()

    override fun <T> interceptContinuation(continuation: Continuation<T>): Continuation<T> =
        object : Continuation<T> {
            override val context: CoroutineContext = continuation.context

            override fun resumeWith(result: Result<T>) {
                dispatchCount.incrementAndGet()
                executor.execute { continuation.resumeWith(result) }
            }
        }
}

private fun <T> runSuspend(
    coroutineContext: CoroutineContext,
    block: suspend () -> T,
): T {
    val finished = CountDownLatch(1)
    var value: Any? = null
    var failure: Throwable? = null
    block.startCoroutine(
        object : Continuation<T> {
            override val context: CoroutineContext = coroutineContext

            override fun resumeWith(result: Result<T>) {
                result.fold(
                    onSuccess = { value = it },
                    onFailure = { failure = it },
                )
                finished.countDown()
            }
        },
    )
    check(finished.await(20, TimeUnit.SECONDS)) { "Kotlin async suite timed out" }
    failure?.let { throw it }
    @Suppress("UNCHECKED_CAST")
    return value as T
}

private fun assertDispatcherThread(stage: String) {
    check(Thread.currentThread().name == DISPATCHER_THREAD) {
        "$stage ran on ${Thread.currentThread().name}, not the Kotlin dispatcher"
    }
}

private class WakeAndReadyKotlinFuture : RustFuture {
    var polls = 0

    override fun poll(wake: Runnable): Any {
        polls += 1
        check(polls == 1) { "completed Kotlin test future was repolled" }
        wake.run()
        wake.run()
        return 99
    }
}

private suspend fun exerciseAsyncInterop() {
    assertDispatcherThread("suite entry")

    val immediate = async_interop.async_interop.immediate(41).await<Int>()
    check(immediate == 42)

    val yielded = async_interop.async_interop.add_after_yield(19, 23).await<Int>()
    check(yielded == 42)

    val chained = async_interop.async_interop.chained(40).await<Int>()
    check(chained == 42)

    async_interop.async_interop.unit_after_yield().await<Unit>()

    // Hundreds of synchronous, repeated wakes exercise coalescing without
    // growing the Kotlin or Java call stack.
    val stormPolls = async_interop.async_interop.synchronous_wake_storm(256).await<Int>()
    check(stormPolls == 257)

    // A wake racing with Ready must not cause an illegal post-completion poll.
    check(async_interop.async_interop.wake_and_ready().await<Int>() == 73)
    val kotlinRace = WakeAndReadyKotlinFuture()
    check(kotlinRace.await<Int>() == 99)
    check(kotlinRace.polls == 1)

    // The wake originates on a Rust-created thread, but Kotlin must resume and
    // repoll through its own ContinuationInterceptor.
    val delayed = async_interop.async_interop.delayed_from_rust_thread(123).await<Int>()
    check(delayed == 123)
    assertDispatcherThread("delayed Rust wake")

    // Multiple Rust threads concurrently update and wake the same future.
    val concurrent = async_interop.async_interop.concurrent_rust_wakers(6).await<Int>()
    check(concurrent == 6)
    assertDispatcherThread("concurrent Rust wakes")

    // The captured value has 64-byte Rust alignment and survives several
    // suspension points, exercising the compiler-emitted future layout.
    check(async_interop.async_interop.aligned_capture(32).await<Int>() == 42)

    // Rich Rust structs and enums travel through a deeply nested async flow.
    val outcome = async_interop.async_interop.extensive_workflow(12, false)
        .await<AsyncOutcome>()
    check(outcome is AsyncOutcome.Completed)
    val report = outcome.field0
    check(report.seed == 12)
    check(report.stages == 4)
    check(report.checksum == 81)
    check(report.aligned)

    val rejected = async_interop.async_interop.extensive_workflow(-7, true)
        .await<AsyncOutcome>()
    check(rejected is AsyncOutcome.Rejected)
    check(rejected.field0 == -7)

    // Completion and exceptional exit both drop captured Rust state once.
    async_interop.async_interop.reset_drop_count()
    check(async_interop.async_interop.tracked_completion(314).await<Int>() == 314)
    check(async_interop.async_interop.drop_count() == 1L)

    async_interop.async_interop.silence_panics()
    async_interop.async_interop.reset_drop_count()
    val panic = try {
        async_interop.async_interop.panic_after_yield().await<Int>()
        null
    } catch (failure: Throwable) {
        failure
    }
    check(panic != null) { "Rust async panic should reach Kotlin as an exception" }
    check(async_interop.async_interop.drop_count() == 1L)

    // Direct abandonment verifies that a pending state machine can be dropped
    // without waiting for a wake that will never arrive.
    async_interop.async_interop.reset_drop_count()
    val abandoned = async_interop.async_interop.abandoned_future()
    check(abandoned.poll(Runnable {}) === KotlinFutureInterop.PENDING)
    KotlinFutureInterop.dropRustFuture(abandoned)
    check(async_interop.async_interop.drop_count() == 1L)

    assertDispatcherThread("suite completion")
}

fun main() {
    val executor = Executors.newSingleThreadExecutor { task ->
        Thread(task, DISPATCHER_THREAD).apply { isDaemon = true }
    }
    val dispatcher = ExecutorDispatcher(executor)
    try {
        runSuspend(dispatcher) { exerciseAsyncInterop() }
        check(dispatcher.dispatchCount.get() >= 4) {
            "Kotlin continuation interceptor was not used for asynchronous wakes"
        }
    } finally {
        executor.shutdownNow()
    }
    println("Kotlin async interop passed")
}
