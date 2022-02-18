package expression;

public class Divide extends Base {
	public Divide(Container var1, Container var2) {
		super(var1, var2, "/");
	}
	
	public int operation(int x, int y) {
		return x / y;
	}
	
	public int status() {
		return 0;
	}
	
	public boolean checker() {
		return true;
	}
}