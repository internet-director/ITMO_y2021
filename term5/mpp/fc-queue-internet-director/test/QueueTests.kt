@file:Suppress("unused")

import org.jetbrains.kotlinx.lincheck.annotations.*
import org.jetbrains.kotlinx.lincheck.paramgen.*

class FlatCombiningQueueTest : AbstractQueueTest(FlatCombiningQueue(), checkObstructionFreedom = false)

@Param(name = "element", gen = IntGen::class, conf = "0:3")
abstract class AbstractQueueTest(
    private val queue: Queue<Int>,
    checkObstructionFreedom: Boolean = true,
    threads: Int = 3,
    actorsBefore: Int = 1
) : TestBase(
    sequentialSpecification = IntQueueSequential::class,
    checkObstructionFreedom = checkObstructionFreedom,
    threads = threads,
    actorsBefore = actorsBefore
) {
    @Operation
    fun enqueue(@Param(name = "element") element: Int) = queue.enqueue(element)

    @Operation
    fun dequeue() = queue.dequeue()

    @Validate
    fun validate() = queue.validate()
}

class IntQueueSequential {
    private val q = ArrayList<Int>()

    fun enqueue(element: Int) {
        q.add(element)
    }

    fun dequeue() = q.removeFirstOrNull()
    fun remove(element: Int) = q.remove(element)
}
