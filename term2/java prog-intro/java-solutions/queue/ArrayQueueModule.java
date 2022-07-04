package queue;

import java.lang.Object;
import java.util.*;

public class ArrayQueueModule {
	// queue[i] != null, i in [0, size]
	private static Object queue[] = new Object[10];
	private static Map<Object, Integer> count = new HashMap<Object, Integer>();
	private static int start = 0;
	private static int end = 0;
	private static int size = 0;
	
	// PRE: obj != null
	// POST: SIZE = size + 1, (i in [0, size] => QUEUE[i] = queue[i]), queue[size + 1] = obj
	public static void enqueue(Object obj) {
		queue[end++] = obj;
		if(!count.containsKey(obj)){
            count.put(obj, 1);
        } else { 
            count.put(obj, count.get(obj) + 1);
        }
		size++;
		fix();
	}

	// PRE: size > 0
	// POST: SIZE = size - 1, (i in [0, size - 1] => QUEUE[i] = queue[i + 1]
    public static Object dequeue() {
		Object o = queue[start++];
        count.put(o, count.get(o) - 1);
		size--;
		fix();
		return o;
	}
	
	// PRE: size > 0
	// POST: return = queue[0]
    public static Object element() {
		return queue[start];
	}
	
	// POST: return = count(obj in queue)
	public static int count(Object obj) {
		if(!count.containsKey(obj)) {
            return 0;
        }
		return count.get(obj);
	}
	
	// POST: return size
	public static int size() {
		return size;
	}
		
	// POST: if size == 0 => true, else => false
	public static boolean isEmpty() {
		return size == 0;
	}
	
	// POST: size = 0
	public static void clear() {
		end = 0;
		start = 0;
		size = 0;
		queue = new Object[10];
		count.clear();
	}	
	
	// POST: end = size, start = 0
	private static void fix() {
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