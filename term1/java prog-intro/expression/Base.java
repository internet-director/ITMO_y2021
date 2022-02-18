package expression;
import java.util.Objects;

public abstract class Base implements Container, TripleExpression, Expression {
	private final Container var1, var2;
	public String o;
	public abstract int operation(int x, int y);
	
	public Base(Container var1, Container var2, String o) {
		this.var1 = var1;
		this.var2 = var2;
		this.o = o;
	}
	
	public String toString() {
		return "(" + var1.toString() + " " + o + " " + var2.toString() + ")";
	}

    public String toMiniString() {
        StringBuilder sb = new StringBuilder();
		if (var1.checker()) {
			sb.append("(" + var1.toMiniString() + " " + o + " " + var2.toMiniString() + ")");
		}
		else {
			sb.append(var1.toMiniString() + " " + o + " " + var2.toMiniString());
		}
        return sb.toString();
    }
	
	public int evaluate(int x) {
		return operation(var1.evaluate(x), var2.evaluate(x));
	}
	
	public int evaluate(int x, int y, int z) {
		return operation(var1.evaluate(x, y, z), var2.evaluate(x, y, z));
	}
	
	public boolean equals(Object obj) {
        if (obj instanceof Base) {
            Base newSecond = (Base) obj;
            return Objects.equals(getClass(), obj.getClass()) &&
                    Objects.equals(var1, newSecond.var1) && Objects.equals(var2, newSecond.var2);
        } else {
            return false;
        }
    }
	
	public int hashCode() {
        return Objects.hash(var1, var2, getClass());
    }
}