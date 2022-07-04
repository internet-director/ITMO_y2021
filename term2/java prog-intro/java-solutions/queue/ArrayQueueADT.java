package queue;

import java.lang.Object;
import java.util.*;

public class ArrayQueueADT {
	// queue[i] != null, i in [0, size]
    private static Object queue[] = new Object[10];
	private static Map<Object, Integer> count = new HashMap<Object, Integer>();
    private static int start = 0;
    private static int end = 0;
    private static int size = 0;

    // PRE: obj != null
	// POST: data.SIZE = data.size + 1, (i in [0, data.size] => data.QUEUE[i] = data.queue[i]), queue[size + 1] = obj
    public static void enqueue(ArrayQueueADT data, Object obj) {
        data.queue[data.end++] = obj;
		if(!data.count.containsKey(obj)){
            data.count.put(obj, 1);
        } else { 
            data.count.put(obj, data.count.get(obj) + 1);
        }
        data.size++;
        fix(data);
    }

    // PRE: data.size > 0
	// POST: data.SIZE = data.size - 1, (i in [0, size - 1] => data.QUEUE[i] = data.queue[i + 1]
    public static Object dequeue(ArrayQueueADT data) {
        Object o = data.queue[data.start++];
		data.count.put(o, data.count.get(o) - 1);
        data.size--;
        fix(data);
        return o;
    }

    // PRE: data.size > 0
	// POST: return = data.queue[0]
    public static Object element(ArrayQueueADT data) {
        return data.queue[data.start];
    }
	
	// POST: return = count(obj in data.queue)
	public static int count(ArrayQueueADT data, Object obj) {
		if(!data.count.containsKey(obj)) {
            return 0;
        }
		return data.count.get(obj);
	}

	// POST: return data.size
    public static int size(ArrayQueueADT data) {
        return data.size;
    }

	// POST: if data.size == 0 => true, else => false
    public static boolean isEmpty(ArrayQueueADT data) {
        return data.size == 0;
    }

	// POST: data.size = 0
    public static void clear(ArrayQueueADT data) {
        data.start = 0;
        data.end = 0;
        data.size = 0;
        data.queue = new Object[10];
		count.clear();
    }

	// POST: end = size, start = 0
    private static void fix(ArrayQueueADT data) {
		// PRE: data.queue.length == data.size
        // POST: data.QUEUE.length = data.queue.length * 2, (data.QUEUE[I] = data.queue[i], i in [0, data.size]), data.SIZE = data.size
        if (size == queue.length) {
            Object o[] = new Object[2 * queue.length];

			// POST: if end < start => o[i] = data.queue[i + start]
			// if start < end => o[i] = data.queue[i + start] and (i + start < data.queue.length) ||
			// o[i] = data.queue[(i + start) % data.queue.length] and (i + start >= data.queue.length)
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