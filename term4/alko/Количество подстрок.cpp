#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>

typedef std::int64_t int_t;
typedef std::uint32_t uint_t;
typedef std::vector<uint_t> bignum;

struct node {
	using list = std::unordered_map<char, int_t>;
	int_t len = 0;
	int_t link = -1;
	list map;

	node() = default;
	node(int_t len, int_t link, list map) : len(len), link(link), map(map) {}
};

int_t dfs(std::vector<bool>& w, std::vector<int_t>& d, std::vector<std::vector<int_t>>& vec, int_t n) {
	if (w[n]) return d[n];
	int_t res = 0;
	w[n] = true;
	for (auto it : vec[n]) res += dfs(w, d, vec, it);
	d[n] = res;
	return res;
}

int main() {
	std::ios::sync_with_stdio(false);
	std::cin.tie(NULL);
	std::cout.tie(NULL);

	std::string s;
	std::cin >> s;
	int_t sz = 1, old_pos = 0, p;
	std::vector<node> states(s.size() * 2 + 1);

	for (char c : s) {
		int_t position = sz++;
		states[position].len = states[old_pos].len + 1;
		for (p = old_pos; p != -1 && !states[p].map.count(c); p = states[p].link) states[p].map[c] = position;
		if (p == -1) states[position].link = 0;
		else {
			int_t st = states[p].map[c], dop;
			if (states[p].len + 1 == states[st].len) states[position].link = st;
			else {
				dop = sz++;
				states[dop] = states[st];
				states[dop].len = states[p].len + 1;
				for (; p != -1 && states[p].map[c] == st; p = states[p].link) states[p].map[c] = dop;
				states[st].link = states[position].link = dop;
			}
		}
		old_pos = position;
	}

	std::vector<std::vector<int_t>> antiG(states.size());

	for (int_t i = 0; i < states.size(); i++) {
		for (auto it : states[i].map)
			antiG[it.second].push_back(i);
	}

	int_t end = 0;
	while (states[end].map.size()) {
		for (auto it : states[end].map) {
			end = it.second;
			break;
		}
	}

	std::vector<bool> w(sz);
	std::vector<int_t> d(sz);
	d[0] = 1;
	w[0] = true;
	dfs(w, d, antiG, end);

	int_t res = 0;
	for (auto it : d) res += it;

	std::cout << res - 1;

	return 0;
}