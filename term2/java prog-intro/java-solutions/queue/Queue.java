package queue;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.*;

public interface Queue {
	// PRE: obj != null
	// POST: SIZE = size + 1, (i in [0, size] => QUEUE[i] = queue[i]), queue[size + 1] = obj
	void enqueue(Object obj);

	// PRE: size > 0
	// POST: SIZE = size - 1, (i in [0, size - 1] => QUEUE[i] = queue[i + 1]
    Object dequeue();
	
	// PRE: size > 0
	// POST: return = queue[0]
    Object element();
	
	// POST: return = count(o in queue : predicate == true)
	int countIf(Predicate<Object> obj);
	
	// POST: return size
	int size();
		
	// POST: if size == 0 => true, else => false
	boolean isEmpty();
	
	// POST: head = tail = null and size = 0
	void clear();
}