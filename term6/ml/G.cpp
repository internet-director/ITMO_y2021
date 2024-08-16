#define _CRT_DISABLE_PERFCRIT_LOCKS

#include <iostream>
#include <algorithm>
#include <vector>

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

using upair = std::pair< int64_t, int64_t>;
using utriple = std::triple< int64_t, int64_t, int64_t>;

int main()
{
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(NULL);

	int64_t n = 0;
	std::cin >> n;

	std::vector<utriple> data1(n), data2(n);
	for (int64_t i = 0; i < n; i++) {
		std::cin >> data1[i].last >> data2[i].last;
		data1[i].first = data2[i].first = i + 1;
	}

	std::sort(data1.begin(), data1.end(), [](const utriple& l, const utriple& r) {
		return l.last > r.last;
		});
	std::sort(data2.begin(), data2.end(), [](const utriple& l, const utriple& r) {
		return l.last > r.last;
		});


	for (int64_t i = 0; i < n; i++) {
		data1[i].middle = data2[i].middle = i + 1;
	}

	std::sort(data1.begin(), data1.end(), [](const utriple& l, const utriple& r) {
		return l.first < r.first;
		});
	std::sort(data2.begin(), data2.end(), [](const utriple& l, const utriple& r) {
		return l.first < r.first;
		});

	uint64_t d = 0;
	for (int64_t i = 0; i < n; i++) {
		int64_t tmp = data1[i].middle - data2[i].middle;
		d += tmp * tmp;
	}

	double result = 1.0 - 6.0 * (double)d / (double)(n * (n * n - 1));
	std::cout << result;

	return 0;
}