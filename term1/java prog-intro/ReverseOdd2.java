import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.*;

public class ReverseOdd2 {
	public static void main(String[] args) throws IOException {
		Scanner in = new Scanner(System.in);
		Scanner dop;
		List<int[]> nInt = new ArrayList<>();
		int countL[] = new int[1];
		int[] num = new int[0];
		
		while (in.hasNextLine()) {
			String str = in.nextLine();
			dop = new Scanner(str);
			num = new int[20];
			int count = 0;
		
			while(dop.hasNextInt()) {
				if(count >= num.length) num = Arrays.copyOf(num, num.length * 2);
				num[count] = dop.nextInt();
				count++;
			}
		
			nInt.add(num);
			if(nInt.size() >= countL.length) {
				countL = Arrays.copyOf(countL, countL.length * 2);
			}
			countL[nInt.size() - 1] = count;
		}
			
		for(int i = nInt.size() - 2; i >= 0; i--) {
			for(int j = countL[i] - 1; j >= 0; j--) {
				if((i + j) % 2 == 1) {
					System.out.print(nInt.get(i)[j] + " ");
				}
			}
			System.out.println();
		}
	}
}