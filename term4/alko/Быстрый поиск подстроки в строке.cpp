#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>

typedef std::int64_t int_t;
typedef std::uint64_t uint_t;
typedef std::vector<uint_t> bignum;

#define MOD (int_t)(1e9 + 7)
#define P 31

int main() {
	std::string str, pattern;
	std::cin >> pattern >> str;
	str = pattern + "___" + str;
	std::vector<int_t> p(str.size()), points;
	int_t l, r, res;
	l = r = res = 0;

	for (int_t i = 1; i < str.size(); i++) {
		if (i <= r) {
			p[i] = std::min(r - i + 1, p[i - l]);
		}
		while (i + p[i] < str.size() && str[p[i]] == str[i + p[i]]) p[i]++;
		if (i + p[i] - 1 > r) {
			l = i;
			r = i + p[i] - 1;
		}
	}

	for (int_t i = pattern.size() + 3; i < p.size(); i++) {
		if (p[i] == pattern.size()) {
			res++;
			points.push_back(i - pattern.size() - 2);
		}
	}

	std::cout << res << "\n";
	for (int_t i : points) std::cout << i << ' ';

	return 0;
}