package search;

public class BinarySearch {
	// PRE: arr.size >= 0
	// PRE: N = arr.size() - 1
	// PRE: arr[i] >= arr[i + 1], 0 <= i < N, arr[j] in Z, 0 <= j <= N
	public static void main(String[] args) {
		int[] arr = new int[args.length - 1];
		
		for (int i = 0; i < args.length - 1; i++) {
			arr[i] = Integer.parseInt(args[i + 1]);
		}
		int index = binIter(arr, Integer.parseInt(args[0]));
		System.out.print(index);
	}
	// POST: print i : arr[i] <= key, arr[i - 1] > key
	
	// PRE: arr[-1] = +INF, arr[N + 1] = -INF, arr[i] >= arr[i + 1], 0 <= i < N, key in Z, arr[j] in Z, 0 <= j <= N
	private static int binIter(int[] arr, int key) {
		int l = -1;
		int r = arr.length;
		
		// INV: arr[r] < key <= arr[l], l = -1, r = N + 1, r - l >= 1
		
		while (r - l > 1) {
			// INV: r - l > 1
			int m = (l + r) / 2;
			// m = mid([l, r]), l < m < r, r - l > 1
			
			if (arr[m] > key) {
				// m = mid([l, r]), r - l > 1, l < m < r, arr[m] > key
				l = m;
				// l = m && (l < r)
			} else {
				// m = mid([l, r]), r - l > 1, l < m < r, arr[m] <= key
				r = m;
				// r = m && (l < r)
			}
		}
		
		// arr[r] < key <= arr[l], (l < r and r - l == 1)
		return r;
	}
	// POST: arr[r] <= key, arr[r - 1] > key
	
	// PRE: arr[-1] = +INF, arr[N + 1] = -INF, arr[i] >= arr[i + 1], 0 <= i < N, key in Z, arr[j] in Z, 0 <= j <= N
	// PRE: l = -1, r = arr.size
	private static int binRec(int[] arr, int key, int l, int r) {
		// PRE: r - l >= 1
		if (r - l == 1) {
			// arr[r] <= key < arr[l], (l < r and r - l == 1)
			return r;
		}
		// POST: r - l > 1
		// arr[r] < key <= arr[l]
		int m = (l + r) / 2;
		// m = mid([l, r]), arr[r] < key <= arr[l], r - l > 1
		
		if (arr[m] > key) {
			// arr[r] < key <= arr[l], r - l > 1, arr[m] > key
			return binRec(arr, key, m, r);
		}
		// arr[r] < key <= arr[l], r - l > 1, arr[m] <= key
		return binRec(arr, key, l, m);
	}
	// POST: arr[r] <= key, arr[r - 1] > key
}