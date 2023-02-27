#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <ctime>
#include <queue>
#include <set>
#include <deque>
#include <algorithm>

namespace std {
	template<typename T1, typename T2, typename T3>
	struct triple {
		triple() = default;
		triple(const T1& t1, const T2& t2, const T3& t3) :a(t1), b(t2), c(t3) {}
		triple(T1&& t1, T2&& t2, T3&& t3) :a(std::move(t1)), b(std::move(t2)), c(std::move(t3)) {}

		T1 a;
		T2 b;
		T3 c;
	};
}

namespace std {
	template<typename T1, typename T2, typename T3, typename T4>
	struct quadruple {
		quadruple() = default;
		quadruple(const T1& t1, const T2& t2, const T3& t3, const T4& t4) :a(t1), b(t2), c(t3), d(t4) {}
		quadruple(T1&& t1, T2&& t2, T3&& t3, T4&& t4) :a(std::move(t1)), b(std::move(t2)), c(std::move(t3)), d(std::move(t4)) {}

		T1 a;
		T2 b;
		T3 c;
		T4 d;
	};
}

using quadruple = std::quadruple<int, int, std::uint64_t, int>;
constexpr int INF = 100000;


int init(std::vector<int>& index, int v) {
	if (index[v] == v) return v;
	return (index[v] = init(index, index[v]));
}

bool dfs(std::vector<std::set<int>>& matrix, int v, std::vector<bool>& visited, std::vector<int>& result, std::vector<int>& dop) {
	if (!visited[v]) {
		visited[v] = true;
		for (int u : matrix[v]) {
			if (dop[u] == -1 || dfs(matrix, dop[u], visited, result, dop)) {
				result[v] = u + 1;
				dop[u] = v;
				return true;
			}
		}
	}
	return false;
}

int main() {
	std::int64_t n, m, s, c = 0;
	std::ifstream in("matching.in");
	std::ofstream out("matching.out");

	in >> n;

	std::vector<int> result(n, 0), dop(n, -1);
	std::vector<bool> visited(n, 0);
	std::vector<std::set<int>> matrix(n);
	std::vector<std::pair<int, int>> w(n);

	for (int i = 0; i < n; i++) {
		int k;
		in >> k;
		w[i] = { k, i };
	}

	for (int i = 0; i < n; i++) {
		int k;
		in >> k;
		for (int j = 0; j < k; j++) {
			int sl;
			in >> sl;
			matrix[i].insert(sl - 1);
			//matrix[sl - 1].insert(i);
		}
	}

	std::sort(w.begin(), w.end(), [](const std::pair<int, int>& a, const std::pair<int, int>& b) {
		return a.first > b.first;
		});

	for (int i = 0; i < n; i++) {
		std::fill(visited.begin(), visited.end(), 0);
		dfs(matrix, w[i].second, visited, result, dop);
	}

	for (auto it : result) {
		out << it << " ";
	}

	return 0;
}