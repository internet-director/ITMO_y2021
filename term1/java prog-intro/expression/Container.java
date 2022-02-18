package expression;
import java.util.Objects;

public interface Container {
	String toString();
	String toMiniString();
	int evaluate(int x);
	int evaluate(int x, int y, int z);
	int status();
	boolean checker();
	//int operation(int x, int y);
	boolean equals(Object obj);
}