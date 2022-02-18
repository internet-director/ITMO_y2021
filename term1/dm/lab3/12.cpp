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
        res += std::to_string(vec[i]) + " ";
    }
    return res;
}
 
void rec(std::vector<std::string>& result, std::vector<int>& vec, int dop) {
    if (dop)
        result.push_back(print(vec, dop));
    int start = (dop) ? vec[dop - 1] + 1 : 1;
    for (int i = start; i <= n; i++) {
        vec[dop] = i;
        rec(result, vec, dop + 1);
    }
}
 
int main() {
    std::cin >> n;
    std::vector<std::string> result;
    std::vector<int> num(n);
    rec(result, num, 0);
    for (std::string p : result) std::cout << "\n" << p;
    return 0;
}