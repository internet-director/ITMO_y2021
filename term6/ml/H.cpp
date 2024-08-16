#define _CRT_DISABLE_PERFCRIT_LOCKS

#include <iostream>
#include <algorithm>
#include <vector>
#include <map>
#include <iomanip>
#include <unordered_map>

namespace std {
	template <typename T1, typename T2, typename T3>
	struct triple {
		constexpr explicit triple() {

		}
		constexpr explicit triple(const T1& a, const T2& b, const T3& c) {
			this->a = a;
			this->b = b;
			this->c = c;
		}

		T1 first;
		T2 middle;
		T3 last;
	};
}

struct my_hash {
	template <class T1, class T2>
	size_t operator()(const std::pair<T1, T2>& p) const
	{
		auto hash1 = std::hash<T1>{}(p.first);
		auto hash2 = std::hash<T2>{}(p.second);

		if (hash1 != hash2) {
			return hash1 ^ hash2;
		}
		return hash1;
	}
};

using upair = std::pair<int64_t, int64_t>;
using utriple = std::triple< int64_t, int64_t, int64_t>;

int main()
{
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(NULL);

	int k1, k2, n;
	std::cin >> k1 >> k2 >> n;

	std::vector<int64_t> K1(k1), K2(k2);
	std::vector<double> _K1(k1), _K2(k2);
	std::unordered_map<upair, int64_t, my_hash> mapa;
	//std::vector<std::vector<int>> matrix(k1, std::vector<int>(k2, 0));

	for (int i = 0; i < n; i++) {
		int x1, x2;
		std::cin >> x1 >> x2;
		//matrix[x1][x2]++;
		mapa[{x1 - 1, x2 - 1}]++;
		K1[x1 - 1]++;
		K2[x2 - 1]++;
	}

	for (int i = 0; i < k1; i++) {
		_K1[i] = (double)K1[i] / (double)n;
	}

	for (int i = 0; i < k2; i++) {
		_K2[i] = (double)K2[i] / (double)n;
	}

	double result = 0.0;
	for (const auto& it : mapa) {
		double s = (double)(_K1[it.first.first] * _K2[it.first.second]) * n;
		double sq = it.second - s;
		result += (sq * sq) / s;
		result -= s;
	}

	std::cout << std::setprecision(12) << result + n;

	return 0;
}