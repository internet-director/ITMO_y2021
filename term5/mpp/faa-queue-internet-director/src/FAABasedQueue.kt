import java.util.concurrent.atomic.*

/**
 * @author Lezhen Stanislav
 */
class FAABasedQueue<E> : Queue<E> {
    private val infiniteArray = AtomicReferenceArray<Any?>(1024) // conceptually infinite array
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)
    private val head = AtomicReference<Segment>()
    private val tail = AtomicReference<Segment>()

    override fun enqueue(element: E) {
        while (true) {
            val i = enqIdx.getAndIncrement()
            if (infiniteArray.compareAndSet(i.toInt(), null, element))
                return
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (!shouldTryToDequeue()) return null
            val i = deqIdx.getAndIncrement()
            if (infiniteArray.compareAndSet(i.toInt(), null, POISONED))
                continue

            if (infiniteArray.compareAndSet(i.toInt(), POISONED, POISONED))
                continue

            val res = infiniteArray.getAndSet(i.toInt(), POISONED) ?: continue
            return res as E
        }
    }

    private fun shouldTryToDequeue(): Boolean {
        while (true) {
            val curDeqIdx = deqIdx
            val curEnqIdx = enqIdx
            if (curDeqIdx != deqIdx)
                continue
            return curDeqIdx.get() < curEnqIdx.get()
        }
    }

    private fun findSegment(index: Int): Segment {

    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2
private val POISONED = Any()