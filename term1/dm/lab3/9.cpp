#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>

int n;

void rec(std::vector<std::string>& result, int dop, int count, std::string& str) {
    if (count == n * 2) {
        if(!dop) result.push_back(str);
    }
    else {
        if (dop < n) {
            str[count] = '(';
            rec(result, dop + 1, count + 1, str);
        }
        if (dop > 0) {
            str[count] = ')';
            rec(result, dop - 1, count + 1, str);
        }
    }
}

int main() {
    std::cin >> n;
    std::vector<std::string> result;
    rec(result, 0, 0, std::string().assign(2 * n, 0));
    for (std::string p : result) std::cout << p << "\n";
    return 0;
}