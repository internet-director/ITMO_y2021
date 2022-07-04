package queue;

import java.lang.Object;
import java.util.function.Predicate;
import java.util.*;

public class ArrayQueue extends AbstractQueue {
	// queue[i] != null, i in [0, size]
    private Object queue[] = new Object[10];
    private int start = 0;
    private int end = 0;

	// PRE: obj != null
	// POST: SIZE = size + 1, (i in [0, size] => QUEUE[i] = queue[i]), queue[size + 1] = obj
    public void enqueue(Object obj) {
        queue[end++] = obj;
		add(obj);
        size++;
        fix();
    }

	// PRE: size > 0
	// POST: SIZE = size - 1, (i in [0, size - 1] => QUEUE[i] = queue[i + 1]
    public Object dequeue() {
        Object o = queue[start++];
		erase(o);
        size--;
        fix();
        return o;
    }

	// PRE: size > 0
	// POST: return = queue[0]
    public Object element() {
        return queue[start];
    }
	

	// POST: size = 0
    public void clear() {
        start = 0;
        end = 0;
        size = 0;
        queue = new Object[10];
		count.clear();
    }

	// POST: end = size, start = 0
    private void fix() {
		// POST: QUEUE.length = queue.length * 2, (QUEUE[I] = queue[i], i in [0, size]), SIZE = size
        if (size == queue.length) {
            Object o[] = new Object[2 * queue.length];

			// POST: if end < start => o[i] = queue[i + start]
			// if start < end => o[i] = queue[i + start] and (i + start < queue.length) ||
			// o[i] = queue[(i + start) % queue.length] and (i + start >= queue.length)
            for (int i = start; i < start + size; i++) {
				o[i - start] = queue[i % queue.length];
			}
            queue = o;
            end = size;
            start = 0;
        }

        end %= queue.length;
        start %= queue.length;
    }
}