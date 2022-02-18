#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>

int n;

std::string print(std::vector<int>& vec) {
    std::string res;
    for (int i = 0; i < vec.size(); i++) {
        res += std::to_string(vec[i]);
    }
    return res;
}

void rec(std::vector<std::string>& result, std::vector<int>& vec, int dop = 0) {
    if (dop == n)
        result.push_back(print(vec));
    else {
        vec[dop] = 0;
        rec(result, vec, dop + 1);
        if (dop < 1 || vec[dop - 1] != 1) {
            vec[dop] = 1;
            rec(result, vec, dop + 1);
        }
    }
}

int main() {
    std::cin >> n;
    std::vector<std::string> result;
    std::vector<int> num(n);
    rec(result, num);
    std::cout << result.size() << "\n";
    for (std::string p : result) std::cout << p << "\n";
    return 0;
}