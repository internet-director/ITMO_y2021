#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>

int n;

std::string print(std::vector<int>& vec, int end) {
    std::string res;
    for (int i = 0; i < end; i++) {
        res += std::to_string(vec[i]) + "+";
    }
    res += std::to_string(vec[end]);
    return res;
}

void rec(std::vector<std::string>& result, std::vector<int>& vec, int dop, int size = n) {
    if (!size)
        result.push_back(print(vec, dop - 1));
    else {
        int start = (dop) ? vec[dop - 1] : 1;
        for (int i = start; i < size + 1; i++) {
            vec[dop] = i;
            rec(result, vec, dop + 1, size - i);
        }
    }
}

int main() {
    std::cin >> n;
    std::vector<std::string> result;
    std::vector<int> num(n);
    rec(result, num, 0);
    for (std::string p : result) std::cout << p << "\n";
    return 0;
}