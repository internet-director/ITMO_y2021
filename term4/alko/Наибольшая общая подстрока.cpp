#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <set>
#include <unordered_set>
#include <algorithm>

typedef std::int32_t int_t;
typedef std::uint32_t uint_t;
typedef std::vector<uint_t> bignum;

#define MOD (int_t)(1e9 + 7 )
#define P 47
#define Q 37

int_t getHash(std::vector<int_t>& hashs, std::vector<int_t>& pows, int_t l, int_t r) {
	return (MOD + (std::int64_t)hashs[r] - ((std::int64_t)hashs[l] * (std::int64_t)pows[r - l]) % MOD) % MOD;
}


int main() {
	std::ios::sync_with_stdio(false);
	std::cin.tie(NULL);
	std::cout.tie(NULL);
	int_t k = 2, maxSz = 0, minIndex = 0;

	std::vector<std::string> str(k);
	std::vector<std::vector<int_t>> hashs_1(k), hashs_2(k);

	for (int_t i = 0; i < k; i++) {
		std::cin >> str[i];
		hashs_1[i].resize(str[i].size() + 1, 0);
		hashs_2[i].resize(str[i].size() + 1, 0);
		maxSz = std::max(maxSz, (int_t)str[i].size());
		if (str[i].size() < str[minIndex].size()) minIndex = i;

		for (int_t j = 0; j < str[i].size(); j++) {
			hashs_1[i][j + 1] = (((std::int64_t)hashs_1[i][j] * (std::int64_t)P) % MOD + str[i][j] - 'a' + 1) % MOD;
			hashs_2[i][j + 1] = (((std::int64_t)hashs_2[i][j] * (std::int64_t)Q) % MOD + str[i][j] - 'a' + 1) % MOD;
		}
	}

	int_t minSz = str[minIndex].size();

	std::vector<int_t> pows1(maxSz + 1, 1), pows2(maxSz + 1, 1);
	for (uint_t i = 0; i < pows1.size() - 1; i++) {
		pows1[i + 1] = ((std::int64_t)pows1[i] * (std::int64_t)P) % MOD;
		pows2[i + 1] = ((std::int64_t)pows2[i] * (std::int64_t)Q) % MOD;
	}

	int_t left = 1, right = minSz + 1;
	int_t i;
	std::string result;
	while (left <= right) {
		i = left + (right - left) / 2;
		bool find = false;
		std::string pos;
		std::unordered_set<int_t> set1, set2;
		for (int_t j = 0; j <= minSz - i; j++) {
			set1.insert(getHash(hashs_1[minIndex], pows1, j, j + i));
			set2.insert(getHash(hashs_2[minIndex], pows2, j, j + i));
		}
		for (int_t j = 0; j < k; j++) {
			if (j == minIndex) continue;
			find = false;
			for (int_t l = 0; l <= (int_t)str[j].size() - i; l++) {
				int_t hash1 = getHash(hashs_1[j], pows1, l, l + i);
				int_t hash2 = getHash(hashs_2[j], pows2, l, l + i);

				if (set1.count(hash1) && set2.count(hash2)) {
					std::string strsss = str[j].substr(l, i);
					find = true;
					if (pos.empty()) pos = strsss;
					pos = std::min(pos, strsss);
				}
			}
			if (!find) break;
		}
		if (find) {
			left = i + 1;
			result = std::move(pos);
		}
		else {
			right = i - 1;
		}
	}

	std::cout << result;
	return 0;
}