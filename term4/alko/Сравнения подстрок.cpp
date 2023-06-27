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
	int m;
	std::string str;
	std::cin >> str >> m;
	std::vector<int_t> pows(str.size() + 1);
	std::vector<int_t> hashs(str.size() + 1);

	pows[0] = 1;
	hashs[0] = 0;

	for (uint_t i = 0; i < str.size(); i++) {
		pows[i + 1] = (pows[i] * P) % MOD;
		hashs[i + 1] = ((hashs[i] * P) % MOD + str[i] - 'a' + 1) % MOD;
	}

	for (int i = 0; i < m; i++) {
		int a, b, c, d;
		std::cin >> a >> b >> c >> d;
		a--;
		c--;

		int_t h1 = (MOD + hashs[b] - (hashs[a] * pows[b - a]) % MOD) % MOD;
		int_t h2 = (MOD + hashs[d] - (hashs[c] * pows[d - c]) % MOD) % MOD;
		int_t m = std::abs(a - c);

		if (h1 == h2 && b - a == d - c && !std::strncmp(str.c_str() + a, str.c_str() + c, b - a)) {
			std::cout << "Yes";
		}
		else {
			std::cout << "No";
		}
		std::cout << "\n";
	}

	return 0;
}