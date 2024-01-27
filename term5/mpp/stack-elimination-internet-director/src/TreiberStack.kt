import java.util.concurrent.atomic.AtomicReference
import kotlin.system.exitProcess

/**
 * @author Lezhen Stanislav
 */
class TreiberStack<E> : Stack<E> {
    private val top = AtomicReference<Node<E>?>(null)

    override fun push(element: E) {
        while(true) {
            val curTop = top.get()
            val newTop = Node(element, curTop)
            if (top.compareAndSet(curTop, newTop)) return
        }
    }

    override fun pop(): E? {
        while(true) {
            val curTop = top.get() ?: return null
            val newTop = curTop.next
            if (top.compareAndSet(curTop, newTop))
                    return curTop.element
        }
    }

    private class Node<E>(
        val element: E,
        val next: Node<E>?
    )
}
