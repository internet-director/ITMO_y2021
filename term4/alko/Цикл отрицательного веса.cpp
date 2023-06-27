#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <set>
#include <map>
#include <deque>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>

namespace std {
	template<typename T1, typename T2, typename T3>
	struct triple {
		triple() = default;
		triple(const T1& t1, const T2& t2, const T3& t3) :a(t1), b(t2), c(t3) {}
		triple(T1& t1, T2& t2, T3& t3) :a(std::move(t1)), b(std::move(t2)), c(std::move(t3)) {}

		T1 a;
		T2 b;
		T3 c;
	};
}

constexpr int INF = 100000;

#define LEFT(X) X.a
#define RIGHT(X) X.b
#define VALUE(X) X.c

int main() {
	int n;
	bool res = false;
	std::cin >> n;

	std::vector<int> d(n), p(n, -1);
	std::unordered_multimap<int, std::pair<int, int>> e;
	std::vector<std::triple<int, int, int>> ee;

	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			int k;
			std::cin >> k;
			if (k != INF) {
				e.insert({ i, {j, k} });
				ee.push_back({ i, j, k });
			}
		}
	}

	int m = ee.size();

	int dop = -1;
	for (int i = 0; i < n; i++) {
		dop = -1;
		for (auto it : ee) {
			if (d[RIGHT(it)] > d[LEFT(it)] + VALUE(it)) {
				d[RIGHT(it)] = std::max(-INF, d[LEFT(it)] + VALUE(it));
				p[RIGHT(it)] = LEFT(it);
				dop = RIGHT(it);
			}
		}
	}

	if (dop == -1) {
	no:;
		std::cout << "NO";
		return 0;
	}

	int k = dop;
	for (int i = 0; i < n && k != -1; i++) k = p[k];
	std::vector<int> result;
	int c = k;
	do {
		if (c == -1) break;
		result.push_back(c);
		c = p[c];
	} while (c != k || result.size() <= 1);
	if (result.empty()) {
		goto no;
	}
	std::cout << "YES" << std::endl << result.size() << std::endl;
	for (int i = result.size() - 1; i >= 0; i--) {
		std::cout << result[i] + 1 << " ";
	}
	return 0;
}