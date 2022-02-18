#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>

int n, m;

std::string print(std::vector<int>& vec) {
    std::string res;
    for (int i = m; i < vec.size(); i++) {
        res += std::to_string(vec[i]) + " ";
    }
    return res;
}

void rec(std::vector<std::string>& result, std::vector<int>& vec, int dop, int last) {
    if (dop == m) {
        result.push_back(print(vec));
    }
    else {
        for (int i = last + 1; i <= n; i++) {
            vec.push_back(i);
            rec(result, vec, dop + 1, i);
            vec.pop_back();
        }
    }
}

int main() {
    std::cin >> n >> m;
    std::vector<int> data;
    std::vector<std::string> result;
    for (int i = 0; i < m; i++) data.push_back(i + 1);
    rec(result, data, 0, 0);
    //std::sort(result.begin(), result.end());
    for (std::string p : result) std::cout << p << "\n";
    return 0;
}