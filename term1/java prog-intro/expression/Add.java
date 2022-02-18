package expression;

public class Add extends Base {
	public Add(Container var1, Container var2) {
		super(var1, var2, "+");
	}
	
	public int operation(int x, int y) {
		return x + y;
	}
	
	public int status() {
		return Integer.MIN_VALUE;
	}
	
	public boolean checker() {
		return false;
	}
}