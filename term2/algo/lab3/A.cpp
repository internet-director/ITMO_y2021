#include <iostream>
#include <string>
#include <vector>
#include <cmath>

#define LN(X) (ceil(log2l(X)))

int main() {
	int n;
	std::cin >> n;
	std::vector<int> data(n);
	std::vector<std::vector<int>> vec(n, std::vector<int>(LN(n), 0));
	for (int i = 0; i < n; i++) {
		std::cin >> data[i];
		vec[i][0] = data[i];
	}

	for (int i = 1; i < LN(n); i++) {
		for (int j = 0; j < n; j++) {
			if (!vec[j][i - 1]) continue;
			vec[j][i] = vec[vec[j][i - 1] - 1][i - 1];
		}
	}

	for (int i = 0; i < n; i++) {
		std::cout << i + 1 << ": ";
		for (int j = 0; j < LN(n); j++) {
			if (!vec[i][j]) break;
			std::cout << vec[i][j] << " ";
		}
		std::cout << "\n";
	}
	return 0;
}