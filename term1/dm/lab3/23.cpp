#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>

std::string last(std::string vec) {
    for (int i = vec.size() - 1; i >= 0; i--) {
        if (vec[i] == '1') {
            vec[i] = '0';
            return vec;
        }
        else vec[i] = '1';
    }
    return (vec = "-");
}
std::string next(std::string vec) {
    for (int i = vec.size() - 1; i >= 0; i--) {
        if (vec[i] == '0') {
            vec[i] = '1';
            return vec;
        }
        else vec[i] = '0';
    }
    return (vec = "-");
}

int main() {
    std::string input;
    std::cin >> input;
    std::cout << last(input) << "\n";
    std::cout << next(input);
    return 0;
}