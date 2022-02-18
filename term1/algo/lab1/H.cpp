#include <iostream>
#include <algorithm>
#include <vector>
#include <string>

void replace(std::string& str, std::string repl) {
	uint32_t p = 0;
	while ((p = str.find(repl, p)) != str.npos) {
		str.replace(p, repl.size(), "");
		p += repl.size();
	}
}

bool check(std::string& str)
{
	std::string s;
	for (char c : str)
	{
		if (c == '(' || c == '{' || c == '[') s += c;
		else if (s.size())
		{
			std::string d;
			d.push_back(s[s.size() - 1]);
			d.push_back(c);
			s.pop_back();
			if (d != "()" && d != "{}" && d != "[]") return false;
		}
		else return false;
	}
	if (s.size()) return false;
	return true;
}

int main() {
	std::string str;
	std::cin >> str;
	if (check(str)) std::cout << "YES";
	else std::cout << "NO";
}