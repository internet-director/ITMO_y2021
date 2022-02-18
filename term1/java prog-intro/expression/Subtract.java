package expression;

public class Subtract extends Base {
	public Subtract(Container var1, Container var2) {
		super(var1, var2, "-");
	}

	public int operation(int x, int y) {
		return x - y;
	}
	
	public int status() {
		return Integer.MIN_VALUE;
	}
	
	public boolean checker() {
		return true;
	}
}