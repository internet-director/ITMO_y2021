#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
std::string print(std::vector<int>& vec) {
    std::string res;
    for (int p : vec) {
        res += '0' + p;
        res += ' ';
    }
    return res;
}
 
void rec(std::vector<std::string>& result, std::vector<int>& vec, int dop) {
    if (dop == vec.size() - 1) {
        result.push_back(print(vec));
    }
    else {
        for (int i = dop; i < vec.size(); i++) {
            std::swap(vec[dop++], vec[i]);
            rec(result, vec, dop--);
            std::swap(vec[dop], vec[i]);
        }
    }
}
 
int main() {
    int n;
    std::cin >> n;
    std::vector<int> data;
    std::vector<std::string> result;
    for (int i = 0; i < n; i++) data.push_back(i + 1);
    rec(result, data, 0);
    std::sort(result.begin(), result.end());
    for (std::string p : result) std::cout << p << "\n";
    return 0;
}