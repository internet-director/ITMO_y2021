#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
std::string next(std::string& str) {
    int close = 0, open = 0;
    for (int i = str.size() - 1; i >= 0; i--) {
        if (str[i] == '(') {
            open++;
            if (close > open) break;
        }
        else close++;
    }
    str.erase(str.begin() + str.size() - open - close, str.end());
    if (str == "") return "-";
    else {
        str += ')';
        for (int j = 0; j < open; j++) str += '(';
        for (int j = 0; j < close - 1; j++) str += ')';
    }
    return str;
}
 
int main() {
    std::string input;
    std::cin >> input;
    std::cout << next(input);
    return 0;
}