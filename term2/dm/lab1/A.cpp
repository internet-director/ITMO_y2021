#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>

struct meta {
	int start;
	int end;
	char c;
};
typedef std::vector<meta> vec;

std::istream& operator>>(std::istream& in, meta& ph)
{
	return in >> ph.start >> ph.end >> ph.c;
}

std::ostream& operator<<(std::ostream& os, const meta& ph)
{
	return os << ph.start << " " << ph.end << " " << ph.c;
}

bool comp(meta& a, meta& b) {
	return a.start < b.start;
}

int find(vec& v, int p, int start = 0) {
	for (int i = start; i < v.size(); i++) {
		if (v[i].start >= p) return (v[i].start > p ? -1 : i);
	}
	return -1;
}

int main() {
	std::ifstream input("problem1.in");
	std::ofstream output("problem1.out");
	//std::ifstream input(stdin);
	//std::ofstream output(stdout);

	std::string word;
	std::vector<int> cond;
	vec tran;
	int n, m, k;
	input >> word >> n >> m >> k;
	cond.resize(k);
	tran.resize(m);
	for (int i = 0; i < k; i++) input >> cond[i];
	for (int i = 0; i < m; i++) input >> tran[i];
	std::sort(tran.begin(), tran.end(), comp);

	int f = 1, s;
	bool res = true;
	for (int i = 0; i < word.size(); i++) {
		s = -1;
		while (true) {
			s = find(tran, f, s + 1);
			if (s == -1) {
				res = false;
				goto end;
			}
			if (tran[s].c == word[i]) {
				f = tran[s].end;
				break;
			}
		}
	}
end:;
	for (int i = 0; i <= cond.size(); i++) {
		if (i == cond.size()) res = false;
		if (f == cond[i]) break;
	}
	output << (res ? "Accepts" : "Rejects");
	return 0;
}