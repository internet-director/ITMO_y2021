public class SumLong {
	public static void main(String[] args)
	{
		long sum = 0;
		StringBuilder num = new StringBuilder("");
		StringBuilder orig = new StringBuilder("");

		for(int i = 0; i < args.length; i++) {
			orig = new StringBuilder(args[i] + " ");
		
			for(int j = 0; j < orig.length(); j++) {
				if(!Character.isWhitespace(orig.charAt(j))) {
					num.append(orig.charAt(j));
				} else {
					if(num.length() != 0) {				
						sum += Long.parseLong(num.toString());
						num.setLength(0);
					}
					//if(orig.charAt(j) == '-') { num = new StringBuilder("-"); }
				}
			}
		}
		System.out.println(sum);
	}
}