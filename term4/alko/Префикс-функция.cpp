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

	for (int_t i = 1; i < str.size(); i++) {
		int_t dop = p[i - 1];
		while (dop && str[i] != str[dop]) dop = p[dop - 1];
		if (str[i] == str[dop]) dop++;
		p[i] = dop;
	}

	for (int_t i : p) std::cout << i << ' ';

	return 0;
}