import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.*;

public class ReverseAbc2 {
	public static void main(String[] args) throws IOException, NumberFormatException {	
		try {
		Scanner in = new Scanner(System.in);
		List<String[]> nStr = new ArrayList<>();
		int countL[] = new int[1];
		String[] str = new String[0];	
		int size = 0;

		while (in.hasNextLine()) {
			String s = in.nextLine();
			Scanner dop = new Scanner(s);
			str = new String[20];
			int count = 0;
		
			while(dop.hasNextWord()) {
				if(count >= str.length) {
					str = Arrays.copyOf(str, str.length * 2);
				}
				str[count] = dop.nextWord();
				count++;
			}
		
			nStr.add(str);
			if(nStr.size() >= countL.length) {
				countL = Arrays.copyOf(countL, countL.length * 2);
			}
			countL[size] = count;
			size++;
		}
			
		for(int i = nStr.size() - 2; i >= 0; i--) {
			for(int j = countL[i] - 1; j >= 0; j--) {
				StringBuilder num = new StringBuilder();
				for(int k = 0; k < nStr.get(i)[j].length(); k++){
					if(nStr.get(i)[j].charAt(k) == '-') {
						num.append(nStr.get(i)[j].charAt(k));
					} else {
						int n = (int)nStr.get(i)[j].charAt(k) - (int)'a';
						num.append(n);
					}
				}
				//System.err.println(Integer.parseInt(num.toString()));
				System.out.print(Integer.parseInt(num.toString()) + " ");
			}
			System.out.println();
		}
		
		} catch(NumberFormatException e) {
			System.err.println("Parse int error");
		} catch(IOException e) {
			System.err.println("Parse string error");
		}
	}
}