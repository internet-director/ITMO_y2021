import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 * @author Lezhen Stanislav
 */

class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    private fun tryLock(): Boolean {
        return combinerLock.compareAndSet(false, true)
    }

    private fun unlock() {
        combinerLock.compareAndSet(true, false)
    }

    override fun enqueue(element: E) {
        runner(Result() {
            queue.add(element)
            null
        })
    }

    override fun dequeue(): E? {
        return runner(Result() {
            if (queue.isEmpty())
                return@Result null
            queue.removeFirst()
        })
    }

    private fun combine() {
        for (index in 0 until TASKS_FOR_COMBINER_SIZE) {
            val _task = tasksForCombiner.get(index) ?: continue
            val task = _task as Result<*>

            if (task.worked) {
                val tmp = Result(task.action)
                tmp.value = task.action() as E?
                tmp.worked = false
                tasksForCombiner.compareAndSet(index, task, tmp)
            }
        }
        unlock()
    }

    private fun runner(oper: Result<E>): E? {
        var index = 0
        while (!tasksForCombiner.compareAndSet(index, null, oper)) {
            index = randomCellIndex()
        }

        while (true) {
            if (tryLock())
                combine()

            val _task = tasksForCombiner.get(index) ?: continue
            val task = _task as Result<*>
            if (!task.worked) {
                tasksForCombiner.compareAndSet(index, task, null)
                return task.value as E
            }
        }
        return null
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

// TODO: Put this token in `tasksForCombiner` for dequeue().
// TODO: enqueue()-s should put the inserting element.
private object Dequeue

// TODO: Put the result wrapped with `Result` when the operation in `tasksForCombiner` is processed.
private class Result<V>(
    val action: () -> V?
) {
    var worked: Boolean = true
    var value: V? = null
}
