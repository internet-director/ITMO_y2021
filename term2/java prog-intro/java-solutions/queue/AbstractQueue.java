package queue;

import java.util.Objects;
import java.util.function.Predicate;
import java.util.*;

public abstract class AbstractQueue implements Queue {
	protected static Map<Object, Integer> count = new HashMap<Object, Integer>();
	protected int size = 0;
	public abstract void enqueue(Object obj);
    public abstract Object dequeue();
    public abstract Object element();
	
	public int count(Object obj) {
		if(!count.containsKey(obj)) {
            return 0;
        }
		return count.get(obj);
	}
	
	public int countIf(Predicate<Object> obj) {
		int counter = 0;
		for (int i = 0; i < size; i++) {
			Object o = dequeue();
			if (obj.test(o)) {
				counter++;
			}
			enqueue(o);
		}
		return counter;
	}
	
    public int size() {
        return size;
    }
	
	public boolean isEmpty() {
		return size == 0;
	}
	
	public abstract void clear();
	
	protected void add(Object obj) {
		if(!count.containsKey(obj)){
            count.put(obj, 1);
        } else { 
            count.put(obj, count.get(obj) + 1);
        }
	}
	
	protected void erase(Object obj) {
		count.put(obj, count.get(obj) - 1);
	}
}
























