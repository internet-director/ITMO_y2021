package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.PriorityBlockingQueue
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors() / 2
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = PriorityBlockingQueue(workers, NODE_DISTANCE_COMPARATOR) // TODO replace me with a multi-queue based PQ!
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    repeat(workers) {
        thread {
            while (true) {
                val node = q.poll() ?: if (q.isEmpty()) break else continue
                for (edge in node.outgoingEdges) {
                    while (true) {
                        val old = edge.to.distance
                        val updated = node.distance + edge.weight
                        if (updated < old) {
                            if (edge.to.casDistance(old, updated)) {
                                q.add(edge.to)
                            } else {
                                continue
                            }
                        }
                        break
                    }
                }
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}