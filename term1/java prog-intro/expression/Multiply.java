package expression;

public class Multiply extends Base {
	public Multiply(Container var1, Container var2) {
		super(var1, var2, "*");
	}
	
	public int operation(int x, int y) {
		return x * y;
	}
	
	public int status() {
		return 0;
	}
	
	public boolean checker() {
		return false;
	}
}