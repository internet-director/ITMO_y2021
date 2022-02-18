#include <iostream>

int main() {
	std::ios::sync_with_stdio(false);
	std::cout.tie(0);
	std::cin.tie(0);
	int n, d, k;
	std::cin >> n >> d;
	for (int i = 1; i < n; i++) {
		std::cin >> k;
		while (k && d) {
			if (k > d) k = k % d;
			else d = d % k;
		}
		d += k;
	}
	std::cout << d;
	return 0;
}