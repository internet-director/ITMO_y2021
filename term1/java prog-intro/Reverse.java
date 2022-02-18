import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Reverse {
	public static void main(String[] args) throws Exception {
		Scanner in = new Scanner(System.in);
		List<int[]> nInt = new ArrayList<>();
		int countL[] = new int[1];
		int[] num = new int[0];
		
		while (in.hasNextLine()) {
			String data = in.nextLine();
			Scanner dop = new Scanner(data);
			num = new int[20];
			int count = 0;			
			//System.err.println(data);
			
			while(dop.hasNextInt()) {
				if(count >= num.length) {
					num = Arrays.copyOf(num, num.length * 2);
				}
				num[count] = dop.nextInt();
				count++;
			}

			nInt.add(num);
			if(nInt.size() >= countL.length) {
				countL = Arrays.copyOf(countL, countL.length * 2);
			}
			countL[nInt.size() - 1] = count;
			dop.close();
		}
		
		//System.err.println(nInt.size());
			
		for(int i = nInt.size() - 2; i >= 0; i--) {
			for(int j = countL[i] - 1; j >= 0; j--) {
				int number = nInt.get(i)[j];
				System.out.print(nInt.get(i)[j] + " ");

			}
			System.out.println();
		}		
		in.close();
	}
}