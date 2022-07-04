#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>

typedef std::map<char, std::vector<std::string>> vpair;

bool rec(vpair vec, std::string word, char start, size_t pos = 0) {
	if (start >= 'a') return word.size() == pos;
	if (pos > word.size() - 1) return false;
	for (std::string p : vec[start]) {
		if (p[0] == word[pos]) {
			if (rec(vec, word, p[p.size() - 1], pos + 1)) return true;
		}
	}
	return false;
}

int main() {
	int n, m;
	std::string dop;
	char start;
	std::ifstream input("automaton.in");
	std::ofstream output("automaton.out");
	//input = std::ifstream(stdin);
	//output = std::ofstream(stdout);

	input >> n >> start;
	vpair vec;

	for (int i = 0; i < n; i++) {
		std::string b;
		char a;
		input >> a >> dop >> b;
		vec[a].push_back(b);
	}
	input >> m;
	for (int i = 0; i < m; i++) {
		input >> dop;
		output << (rec(vec, dop, start) ? "yes" : "no") << "\n";
	}
	
	return 0;
}