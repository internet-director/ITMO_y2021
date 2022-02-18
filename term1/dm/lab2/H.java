import java.io.*;
import java.util.*;
import java.math.*;

public class G {
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int n = in.nextInt();
		String alph = new String();
		
		long count[] = new long[n];
		BigDecimal pred[] = new BigDecimal[n + 1];
		Arrays.fill(pred, BigDecimal.ZERO);
		
		for (int i = 0; i < n; i++) {
			alph += (char)('a' + i);
		}
		long sum = 0;
		for (int i = 0; i < n; i++) {
			count[i] = in.nextInt();
			sum += count[i];
			//count[str.charAt(i) - 'a']++;
		}
		
		String str = in.next(), qs = "1";
		for (int i = 0; i < str.length(); i++) {
			qs += "0";
		}
		BigInteger p = new BigInteger(str, 2);
		BigInteger q = new BigInteger(qs, 2);
		BigDecimal k = new BigDecimal(sum);
		
		BigDecimal dr = new BigDecimal(p);
		dr = dr.divide(new BigDecimal(q));
		
		for (int i = 0; i < n; i++) {
			pred[i + 1] = BigDecimal.valueOf(count[i]);
			pred[i + 1] = pred[i + 1].divide(k, 26, BigDecimal.ROUND_DOWN);
			pred[i + 1] = pred[i + 1].add(pred[i]);
		}

		BigDecimal lefB = new BigDecimal("0");
		BigDecimal rigB = new BigDecimal("1");
		BigDecimal dop = new BigDecimal("0");
		BigDecimal l = new BigDecimal("0");
		
		BigDecimal left = new BigDecimal("0");
		BigDecimal right = new BigDecimal("1");
		BigDecimal r = new BigDecimal("1");
		
		String result = new String();
		
		for (int i = 0; i < sum; i++) {
			dop = rigB;
			dop = dop.subtract(lefB);
			
			int j = 0;
			for (int kk = 0; kk < n; kk++) {
				right = dop;
				right = right.multiply(pred[kk + 1]);
				right = right.add(lefB);
				
				r = lefB;
				left = dop;
				left = left.multiply(pred[kk]);
				left = left.add(r);
				
				//System.out.println(dr.doubleValue() + "  " + left.doubleValue() + "  " + right.doubleValue());
				
				if (left.compareTo(dr) <= 0 && right.compareTo(dr) > 0) {
					j = kk;
					break;
				}
			}
			result += alph.charAt(j);
			
			rigB = dop;
			rigB = rigB.multiply(pred[j + 1]);
			rigB = rigB.add(lefB);
			
			l = lefB;
			lefB = dop;
			lefB = lefB.multiply(pred[j]);
			lefB = lefB.add(l);
		}
		System.out.print(result);
	}
}