#include <algorithm>
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int main() {
	int n;
	std::cin >> n;
	std::vector<int> prime;
	for (int i = 2; i <= n; i++) {
		while (n % i == 0) {
			prime.push_back(i);
			n /= i;
		}
	}
	for (int i : prime) std::cout << i << " ";
	return 0;
}