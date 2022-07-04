#include <iostream>
#include <algorithm>
#include <string>
#include <vector>

bool comp(long long a, long long b) {
	return a > b;
}

int main() {
	size_t n, root;
	std::cin >> n;
	std::vector<long long> data(n);

	for (int i = 0; i < n; i++) {
		std::cin >> data[i];
	}
	std::sort(data.begin(), data.end(), comp);
	std::cout << n << "\n";
	for (int i = 0; i < n - 1; i++) {
		std::cout << data[i] << " " << i + 2 << " " << -1 << "\n";
	}
	std::cout << data[n - 1] << " " << -1 << " " << -1 << "\n";
	std::cout << 1;
	return 0;
}