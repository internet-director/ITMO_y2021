#include <iostream>
#include <vector>
#include <string>
#include <map>

int main() {
    std::string str;
    std::map <std::string, int> alph;
    std::vector <long long> result;
    std::cin >> str;

    //std::vector<std::pair<std::string, int>> alph;
    std::string dop;
    for (int i = 0; i < 26; i++) {
        dop = 'a' + i;
        alph[dop] = i;
        //alph.push_back(std::make_pair(example, i));
    }

    dop.clear();
    int count = 25;
    for (int i = 0; i < str.size(); i++) {
        dop.push_back(str[i]);
        if (alph.find(dop) == alph.end()) {
            result.push_back(alph[dop.substr(0, dop.size() - 1)]);
            alph[dop] = ++count;
            dop = str[i];
        }
    }
    result.push_back(alph[dop]);

    for (int i = 0; i < result.size(); i++) std::cout << result[i] << " ";
    return 0;
}