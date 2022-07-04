package queue;
import java.util.Objects;
import java.util.function.Predicate;

public class LinkedQueue extends AbstractQueue {
	private Node end;
	private Node start;
		
	// PRE: obj != null
	// POST: SIZE = size + 1, (i in [0, size] => QUEUE[i] = queue[i]), queue[size + 1] = obj
	public void enqueue(Object obj) {
		Node tmp = end;
		end = new Node(obj);
		if (isEmpty()) {
			start = end;
		} else {
			tmp.next = end;
		}
        size++;
    }

    // PRE: data.size > 0
	// POST: data.SIZE = data.size - 1, (i in [0, size - 1] => data.QUEUE[i] = data.queue[i + 1]
    public Object dequeue() {
		Object o = start.data;
		start = start.next;
        size--;
        return o;
    }
	

	// PRE: data.size > 0
	// POST: return = data.queue[0]
    public Object element() {
        return start.data;
    }

	// POST: size = 0, end = start = null
    public void clear() {
		end = start = null;
        size = 0;
    }
	
	private class Node {
		public Object data;
		public Node next;
		
		// POST: ths.data = data, next = null
		public Node(Object data) {
			this.data = data;
			this.next = null;
		}
	}
}
























