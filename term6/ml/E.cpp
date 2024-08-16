#define _CRT_DISABLE_PERFCRIT_LOCKS

#include <iostream>
#include <cmath>
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

	int Kx, Ky, N;
	std::cin >> Kx >> Ky >> N;

	std::vector<int64_t> X(Kx, 0), Y(Ky, 0);
	std::unordered_map<upair, int64_t, my_hash> mapa;

	for (int i = 0; i < N; i++) {
		int64_t x, y;
		std::cin >> x >> y;
		mapa[{x - 1, y - 1}]++;
		X[x - 1]++;
		Y[y - 1]++;
	}

	double result = 0.0;

	for (const auto& it : mapa) {
		auto first = double(it.second) / double(N);
		auto second = double(first) / (double(X[it.first.first]) / double(N));

		result -= first * std::log(second);
	}

	std::cout << std::setprecision(12) << result << std::endl;


	return 0;
}