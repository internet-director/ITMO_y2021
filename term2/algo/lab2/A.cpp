#include <iostream>
#include <string>
#include <vector>

typedef std::vector<std::vector<long long>> v;

bool check(long long root, v& p, long long l, long long r) {
	if (root == -1) return true;
	if (p[root][0] < l || p[root][0] > r) return false;
	return check(p[root][1], p, l, p[root][0] - 1) && check(p[root][2], p, p[root][0] + 1, r);
}

int main() {
	size_t n, root;
	std::cin >> n;
	v vec(n + 1, std::vector<long long>(3, 0));

	for (int i = 1; i <= n; i++) {
		for (int j = 0; j < 3; j++) {
			std::cin >> vec[i][j];
		}
	}
	std::cin >> root;

	if (check(root, vec, LLONG_MIN, LLONG_MAX)) {
		std::cout << "YES";
	}
	else {
		std::cout << "NO";
	}
	return 0;
}