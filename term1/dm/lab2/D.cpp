#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

int main() 
{
	std::string str, alph;
	for (int i = 0; i < 26; i++) alph += 'a' + i;
	std::cin >> str;

	for (int i = 0; i < str.size(); i++) {
		int position = alph.find(str[i]);
		alph.insert(alph.begin(), alph[position]);
		alph.erase(++position, 1);
		std::cout << position << " ";
	}
	return 0;
}