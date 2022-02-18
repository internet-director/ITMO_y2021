#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

int main() 
{
	std::string str, result;
	std::cin >> str;
	//std::cin >> str;
	std::vector<std::string> vec(str.size());


	for (int j = 0; j < vec.size(); j++) {
		for (int i = 0; i < vec.size(); i++) {
			vec[i].insert(vec[i].begin(), str[i]);
		}
		std::sort(vec.begin(), vec.end());
	}

	std::cout << vec[0];
	return 0;
}