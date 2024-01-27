@file:Suppress("DuplicatedCode", "FoldInitializerAndIfToElvis")

import java.util.concurrent.atomic.*

/**
 * @author Lezhen Stanislav
 */

class MSQueueWithConstantTimeRemove<E> : QueueWithRemove<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(null, null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        // TODO: When adding a new node, check whether
        // TODO: the previous tail is logically removed.
        // TODO: If so, remove it physically from the linked list.


        while (true) {
            val curTail = tail.get()
            val node = Node(element, curTail)

            if (curTail.next.compareAndSet(null, node)) {
                tail.compareAndSet(curTail, node)
                if (curTail.extractedOrRemoved)
                    curTail.remove()
                return
            } else {
                tail.compareAndSet(curTail, curTail.next.get())
                if (curTail.extractedOrRemoved)
                    curTail.remove()
            }
        }
    }

    override fun dequeue(): E? {
        // TODO: After moving the `head` pointer forward,
        // TODO: mark the node that contains the extracting
        // TODO: element as "extracted or removed", restarting
        // TODO: the operation if this node has already been removed.

        while (true) {
            val curHead = head.get()
            val curHeadNext = curHead.next.get() ?: return null

            if (head.compareAndSet(curHead, curHeadNext)) {
                curHeadNext.prev.set(null)
                if (curHeadNext.markExtractedOrRemoved()) {
                    val res = curHeadNext.element
                    curHeadNext.element = null
                    return res
                }
            }
        }
    }

    override fun remove(element: E): Boolean {
        // Traverse the linked list, searching the specified
        // element. Try to remove the corresponding node if found.
        // DO NOT CHANGE THIS CODE.
        var node = head.get()
        while (true) {
            val next = node.next.get()
            if (next == null) return false
            node = next
            if (node.element == element && node.remove()) return true
        }
    }

    /**
     * This is an internal function for tests.
     * DO NOT CHANGE THIS CODE.
     */
    override fun validate() {
        check(head.get().prev.get() == null) {
            "`head.prev` must be null"
        }
        check(tail.get().next.get() == null) {
            "tail.next must be null"
        }
        // Traverse the linked list
        var node = head.get()
        while (true) {
            if (node !== head.get() && node !== tail.get()) {
                check(!node.extractedOrRemoved) {
                    "Removed node with element ${node.element} found in the middle of the queue"
                }
            }
            val nodeNext = node.next.get()
            // Is this the end of the linked list?
            if (nodeNext == null) break
            // Is next.prev points to the current node?
            val nodeNextPrev = nodeNext.prev.get()
            check(nodeNextPrev != null) {
                "The `prev` pointer of node with element ${nodeNext.element} is `null`, while the node is in the middle of the queue"
            }
            check(nodeNextPrev == node) {
                "node.next.prev != node; `node` contains ${node.element}, `node.next` contains ${nodeNext.element}"
            }
            // Process the next node.
            node = nodeNext
        }
    }

    private inner class Node<E>(
        var element: E?,
        prev: Node<E>?
    ) {
        val next = AtomicReference<Node<E>?>(null)
        val prev = AtomicReference(prev)

        /**
         * TODO: Both [dequeue] and [remove] should mark
         * TODO: nodes as "extracted or removed".
         */
        private val _extractedOrRemoved = AtomicBoolean(false)
        val extractedOrRemoved
            get() =
                _extractedOrRemoved.get()

        fun markExtractedOrRemoved(): Boolean =
            _extractedOrRemoved.compareAndSet(false, true)

        /**
         * Removes this node from the queue structure.
         * Returns `true` if this node was successfully
         * removed, or `false` if it has already been
         * removed by [remove] or extracted by [dequeue].
         */
        fun remove(): Boolean {
            val result = markExtractedOrRemoved()

            if (this == head.get() && this != tail.get()) {
                return result
            }

            val curNext = next.get()
            val curPrev = prev.get()

            if (curNext != null && curPrev != null) {
                curPrev.next.set(curNext)
                setPrev(curNext, curPrev)
                if (curNext.extractedOrRemoved) curNext.remove()
                if (curPrev.extractedOrRemoved) curPrev.remove()
            }
            return result
        }

        private fun setPrev(curNext: Node<E>, curPrev: Node<E>) {
            var curPrevNext = curNext.prev.get()

            while (curPrevNext != null && !curNext.prev.compareAndSet(curPrevNext, curPrev)) {
                curPrevNext = curNext.prev.get()
            }
        }
    }
}