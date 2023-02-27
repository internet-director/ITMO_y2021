#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <set>
#include <deque>
#include <algorithm>

bool comp(const std::pair<int, int>& a, const std::pair<int, int>& b) {
	if (a.second > b.second) return true;
	if (a.second < b.second) return false;
	if (a.first > b.first) return true;
	return false;
}

int main() {
	std::uint64_t n, fine = 0;
	std::ifstream in("schedule.in");
	std::ofstream out("schedule.out");
	in >> n;
	std::vector<std::pair<int, int>> vec(n);
	std::set<int> deque;

	for (int i = 0; i < n; i++) {
		in >> vec[i].first >> vec[i].second;
		deque.insert(i + 1);
	}

	std::sort(vec.begin(), vec.end(), [](const std::pair<int, int>& a, const std::pair<int, int>& b) {
		return a.second > b.second;
		});

	for (int i = 0; i < n; i++) {
		auto it = deque.lower_bound(vec[i].first);
		if (it == deque.begin()) {
			if (*it <= vec[i].first) deque.erase(it);
			else {
				fine += vec[i].second;
				deque.erase(--deque.end());
			}
		}
		else {
			if (it != deque.end()) {
				if (*it == vec[i].first) deque.erase(it);
				else deque.erase(--it);
			} else deque.erase(--it);
		}
	}

	out << fine;

	return 0;
}