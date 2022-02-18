import java.io.*;
import java.util.*;
import java.math.*;

public class G {
	public static void main(String[] args) {
		BigDecimal lefB = new BigDecimal("0");
		BigDecimal rigB = new BigDecimal("1");
		BigDecimal dop = new BigDecimal("0");
		BigDecimal l = new BigDecimal("0");
		BigDecimal two = new BigDecimal("2");
		BigDecimal stepB = new BigDecimal("1");
		BigDecimal powB = new BigDecimal("2");
		BigDecimal res = new BigDecimal("0");
		
		Scanner in = new Scanner(System.in);
		int n = in.nextInt();
		String str = in.next();
		String alph = new String();
		
		long count[] = new long[n];
		BigDecimal pred[] = new BigDecimal[n + 1];
		Arrays.fill(pred, BigDecimal.ZERO);
		
		for (int i = 0; i < n; i++) {
			alph += (char)('a' + i);
		}
		for (int i = 0; i < str.length(); i++) {
			count[str.charAt(i) - 'a']++;
		}
		
		double len = str.length();
		BigDecimal k = new BigDecimal(len);
		
		for (int i = 0; i < n; i++) {
			pred[i + 1] = BigDecimal.valueOf(count[i]);
			pred[i + 1] = pred[i + 1].divide(k, 26, BigDecimal.ROUND_DOWN);
			pred[i + 1] = pred[i + 1].add(pred[i]);
		}
		
		for (int i = 0; i < str.length(); i++) {
			int j = str.charAt(i) - 'a';
			dop = rigB;
			dop = dop.subtract(lefB);
						
			rigB = dop;
			rigB = rigB.multiply(pred[j + 1]);
			rigB = rigB.add(lefB);
			
			l = lefB;
			lefB = dop;
			lefB = lefB.multiply(pred[j]);
			lefB = lefB.add(l);
		}
		
		while (true) {
			res = BigDecimal.ONE;
			res = res.multiply(lefB);
			res = res.multiply(powB);
			res = res.setScale(0, BigDecimal.ROUND_DOWN);
			
			do {
				dop = res.divide(powB);
				res = res.add(BigDecimal.ONE);
				
			} while(res.compareTo(powB) <= 0 && dop.compareTo(lefB) < 0);

			if (res.compareTo(powB) <= 0 && (dop.compareTo(lefB) >= 0 && dop.compareTo(rigB) < 0)) break;
			
			stepB = stepB.add(BigDecimal.ONE);
			powB = powB.multiply(two);
		}
		
		res = res.subtract(BigDecimal.ONE);
		res = res.setScale(0, BigDecimal.ROUND_DOWN);
		BigInteger resultI = res.toBigInteger();

		System.out.println(n);
		for (int i = 0; i < n; i++) {
			System.out.print(count[i] + " ");
		}
		System.out.println();
		
		String s = resultI.toString(2);
		for (int i = 0; i < stepB.intValue() - s.length(); i++) {
			System.out.print('0');
		}
		System.out.print(s);
	}
}