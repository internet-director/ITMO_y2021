interface Stack<E> {
    /**
     * Adds the specified [element] to the top of this stack.
     */
    fun push(element: E)

    /**
     * Retrieves the top element from this stack and returns it;
     * returns `null` if the stack is empty.
     */
    fun pop(): E?
}