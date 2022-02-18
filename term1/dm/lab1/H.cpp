#include <iostream>
#include <cmath>
#include <vector>
#include <string>
#include <Windows.h>
#include <fstream>

#define _I(X) std::to_string(X)

int h_n;

std::string rec(std::string str, int count) {
    if (count == h_n) {
        return str;
    }
    return rec("((" + str + "|((A" + _I(count) + "|A" + _I(count) + ")|(B" + _I(count) + "|B" + _I(count) + ")))|(A" + _I(count) + "|B" + _I(count) + "))", count + 1);
}

int main() {
    //h_n = 3;
    std::cin >> h_n;
    std::string st = "((((A0|B0)|(A0|B0))|((A1|A1)|(B1|B1)))|(A1|B1))";

    std::cout << rec("((A0|B0)|(A0|B0))", 1);
    return 0;
}