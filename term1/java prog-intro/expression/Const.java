package expression;
import java.util.Objects;

public class Const implements Container, TripleExpression, Expression {
	private final int number;
	
	public Const(int number) {
		this.number = number;
	}
	
	public String toString() {
		return String.valueOf(number);
	}
	
	public String toMiniString() {
		return String.valueOf(number);
	}
	
	public int evaluate(int x) {
		return number;
	}
	
	public int evaluate(int x, int y, int z) {
		return number;
	}
	
	public boolean equals(Object obj) {
        if (obj instanceof Const) {
            Const dop = (Const) obj;
            return number == dop.number;
        } else {
            return false;
        }
	}
	
	public int hashCode() {
        return Integer.hashCode(number);
    }
		
	public int status() {
		return Integer.MAX_VALUE;
	}
	
	public boolean checker() {
		return false;
	}
}