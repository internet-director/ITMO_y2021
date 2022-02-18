#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>

int main() {
	long n, x, y, m, result;
	std::cin >> n >> x >> y;

	if (x < y) std::swap(x, y);
	long l = 0, r = (n - 1) * x;

	while (r - l > 1) {
		m = (l + r) / 2;
		if (m / x + m / y < n - 1) l = m;
		else r = m;
	}
	std::cout << r + y;
	return 0;
}