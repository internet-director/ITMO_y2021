#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

int main() 
{
	std::string str, result;
	std::cin >> str;
	std::vector<std::string> vec(str.size());

	for (int i = 0; i < vec.size(); i++) {
		vec[i] = str;
		std::rotate(str.begin(), str.begin() + 1, str.end());
	}

	std::sort(vec.begin(), vec.end());

	for (int i = 0; i < vec.size(); i++) {
		result.push_back(vec[i][vec.size() - 1]);
	}
	std::cout << result;
	return 0;
}