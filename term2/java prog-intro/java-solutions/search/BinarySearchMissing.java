package search;

public class BinarySearchMissing {
	// PRE: arr.size >= 0
	// PRE: N = arr.size - 1
	// PRE: arr[i] <= arr[i + 1], 0 <= i < N, arr[j] in Z, 0 <= j <= N
	public static void main(String[] args) {
		int[] arr = new int[args.length - 1];
		for (int i = 0; i < args.length - 1; i++) {
			arr[i] = Integer.parseInt(args[i + 1]);
		}
		//System.out.print(binIter(arr, Integer.valueOf(args[0])));
		System.out.println(binRec(arr, Integer.valueOf(args[0]), -1, arr.length));
	}
	// POST: print i : arr[i] <= key, arr[i - 1] > key

	// PRE: arr[-1] = -INF, arr[N + 1] = +INF, arr[i] <= arr[i + 1], 0 <= i < N, key in Z, arr[j] in Z, 0 <= j <= N
	private static int binIter(int[] arr, int key) {
		int l = -1;
		int r = arr.length;
		// INV: arr[l] < key <= arr[r], l = -1, r = N + 1, r - l >= 1

		while (r - l > 1) {
			// INV: r - l > 1
			int m = (l + r) / 2;
			// m = mid([l, r]), l < m < r

			if (arr[m] < key) {
				// m = mid([l, r]), r - l > 1, l < m < r, arr[m] < key
				l = m;
				// l = m && (l < r)
			} else {
				// m = mid([l, r]), r - l > 1, l < m < r, arr[m] >= key
				r = m;
				// r = m && (l < r)
			}
		}

		// arr[l] < key <= arr[r], (l < r and r - l == 1)
		return missing(arr, key, r);
	}
	// POST: arr[r] <= key, arr[r + 1] > key


	// PRE: arr[-1] = -INF, arr[N + 1] = +INF, arr[i] <= arr[i + 1], 0 <= i < N, key in Z, arr[j] in Z, 0 <= j <= N
	// PRE: l = -1, r = arr.size
	private static int binRec(int[] arr, int key, int l, int r) {
		// PRE: r - l >= 1
		if (r - l <= 1) {
			// arr[l] < key <= arr[r], (l < r and r - l == 1)
			return missing(arr, key, r);
		}
		// POST: r - l > 1
		// arr[l] < key <= arr[r]
		int m = (l + r) / 2;
		// m = mid([l, r]), arr[l] < key <= arr[r], r - l > 1

		if (arr[m] < key) {
			// arr[l] < key <= arr[r], r - l > 1, arr[m] < key
			return binRec(arr, key, m, r);
		}
		// arr[l] < key <= arr[r], r - l > 1, arr[m] >= key
		return binRec(arr, key, l, m);
	}
	// POST: arr[r] <= key and r = min(R : arr[R] <= key)

	// PRE: arr[l] < key <= arr[r]
	private static int missing(int[] arr, int key, int m) {
		if (arr.length == 0 || key < arr[0]) {
			return -1;
		} else if (m < arr.length && arr[m] == key) {
			return m;
		}
		return -m - 1;
	}
	// POST: if arr.size == 0 or key < arr[0] => r = -1
	// POST: else if arr.size > m and arr[m] == key => r = m
	// POST: else => r = -m -1
}