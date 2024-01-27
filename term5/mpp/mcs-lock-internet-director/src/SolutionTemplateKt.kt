import java.util.concurrent.atomic.*

class Solution(val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node>()

    override fun lock(): Node {
        val my = Node() // сделали узел
        my.lock()
        tail.getAndSet(my)?.let {
            it.next.set(my)
            while (my.locked.get())
                env.park()
        }
        return my // вернули узел
    }

    override fun unlock(node: Node) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null))
                return
            while (node.next.get() == null) {
            }
        }
        node.next.get().unlock()
        env.unpark(node.next.get().thread)
    }

    class Node {
        val thread = Thread.currentThread()
        val locked = AtomicReference<Boolean>(false)
        val next = AtomicReference<Node>(null)

        fun lock() {
            locked.set(true)
        }

        fun unlock() {
            locked.set(false)
        }
    }
}