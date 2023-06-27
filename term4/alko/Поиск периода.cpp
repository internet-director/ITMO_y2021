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
	std::string str;
	std::cin >> str;
	std::vector<int_t> p(str.size());
	int_t l, r;
	l = r = 0;

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

	for (int_t i = 1; i < p.size(); i++)
		if (i + p[i] == str.size() && p.size() % i == 0) {
			std::cout << i;
			return 0;
		}
	std::cout << str.size();
	return 0;
}