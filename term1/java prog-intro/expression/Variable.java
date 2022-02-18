package expression;
import java.util.Objects;

public class Variable implements Container, TripleExpression, Expression {
	private final String var;
	
	public Variable(String var) {
		this.var = var;
	}
	
	public String toString() {
		return var;
	}
	public String toMiniString() {
		return var;
	}
	
	public int evaluate(int x) {
		return x;
	}
	
	public int evaluate(int x, int y, int z) {
		if (var == "x") {
			return x;
		} else if (var == "y") {
			return y;
		} else {
			return z;
		}
	}
	
	public boolean equals(Object obj) {
        if (obj instanceof Variable) {
            Variable dop = (Variable) obj;
            return Objects.equals(var, dop.var);
        } else {
            return false;
        }
    }
	
    public int hashCode() {
        return Objects.hashCode(var);
    }
	
	public int status() {
		return Integer.MAX_VALUE;
	}
	
	public boolean checker() {
		return false;
	}
}