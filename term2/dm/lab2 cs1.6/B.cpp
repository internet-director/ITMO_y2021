#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <stack>
#include <set>
#include <map>

struct meta {
	char c;
	std::vector<char> str;
	meta(std::string& str) {
		c = str[0];
		if (str.size() < 5)
			return;
		for (int i = 0; i < str.size() - 5; i++)
			this->str.push_back(str[i + 5]);
	}
};

int main() {
	int n, m;
	char start;
	std::ifstream input("epsilon.in");
	std::ofstream output("epsilon.out");
	//input = std::ifstream(stdin);
	//output = std::ofstream(stdout);

	input >> n >> start;
	std::vector<char> term, dop;
	std::vector<meta> eps;

	for (int i = 0; i < n; i++) {
		std::string b;
		std::getline(input, b);
		if (b == "") {
			i--;
			continue;
		}

		if (b.size() < 6) {
			term.push_back(b[0]);
			dop.push_back(b[0]);
		}

		eps.push_back(b);
	}

	while (term.size()) {
		char c = term[term.size() - 1];
		term.pop_back();
		for (int i = 0; i < eps.size(); i++) {
			std::vector<char> tmp;
			for (int j = 0; j < eps[i].str.size(); j++) {
				if (c == eps[i].str[j]) {

				}
				else {
					tmp.push_back(eps[i].str[j]);
				}
			}
			eps[i].str = tmp;
			if (!tmp.size() && (std::find(dop.begin(), dop.end(), eps[i].c) == dop.end())) {
				term.push_back(eps[i].c);
				dop.push_back(eps[i].c);
			}
		}
	}

	std::sort(dop.begin(), dop.end());

	for (auto p : dop) {
		output << p << " ";
	}

	return 0;
}