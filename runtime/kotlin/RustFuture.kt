package org.rustlang.runtime

import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlin.coroutines.suspendCoroutine

private sealed class PollSignal

private class Ready(val value: Any?) : PollSignal()

private object Repoll : PollSignal()

/**
 * Arbitrates wakes which race with a poll. A wake observed during the poll is
 * deferred until its result is known, so a future which wakes and returns
 * Ready is never polled again after completion.
 */
private class PollGate(
    private val continuation: Continuation<PollSignal>,
) : Runnable {
    private var polling = true
    private var wakeRequested = false
    private var resolved = false

    override fun run() {
        val resume = synchronized(this) {
            when {
                resolved -> false
                polling -> {
                    wakeRequested = true
                    false
                }
                else -> {
                    resolved = true
                    true
                }
            }
        }
        if (resume) {
            continuation.resume(Repoll)
        }
    }

    fun finish(result: Any?) {
        val signal = synchronized(this) {
            polling = false
            when {
                resolved -> null
                result !== KotlinFutureInterop.PENDING -> {
                    resolved = true
                    Ready(result)
                }
                wakeRequested -> {
                    resolved = true
                    Repoll
                }
                else -> null
            }
        }
        if (signal != null) {
            continuation.resume(signal)
        }
    }

    fun fail(failure: Throwable) {
        val resume = synchronized(this) {
            polling = false
            if (resolved) {
                false
            } else {
                resolved = true
                true
            }
        }
        if (resume) {
            continuation.resumeWithException(failure)
        }
    }
}

private suspend fun RustFuture.pollAfterWake(): PollSignal =
    suspendCoroutine { continuation ->
        val gate = PollGate(continuation)
        try {
            gate.finish(poll(gate))
        } catch (failure: Throwable) {
            gate.fail(failure)
        }
    }

/**
 * Awaits a Rust `async` value. Every repoll is dispatched through the current
 * Kotlin coroutine context; no global Java executor is imposed by the bridge.
 */
@Suppress("UNCHECKED_CAST")
suspend fun <T> RustFuture.await(): T {
    try {
        while (true) {
            when (val signal = pollAfterWake()) {
                Repoll -> continue
                is Ready -> return (
                    if (signal.value === KotlinFutureInterop.UNIT) Unit else signal.value
                ) as T
            }
        }
    } finally {
        KotlinFutureInterop.dropRustFuture(this)
    }
}
