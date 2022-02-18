#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>

int main() {
	long long n, k;
	std::cin >> n >> k;
	std::vector<long long> a(n), b(k);

	for (long i = 0; i < n; i++) {
		std::cin >> a[i];
	}
	for (long i = 0; i < k; i++) {
		std::cin >> b[i];
	}

	for (long long p : b) {
		long long l = 0, r = n, m;

		while (r - l > 1) {
			m = (r + l) / 2;
			if (a[m] > p) r = m;
			else l = m;
		}
		if (abs(p - a[l]) > abs(p - a[r])) std::cout << a[r];
		else std::cout << a[l];
		std::cout << "\n";
	}

	return 0;
}