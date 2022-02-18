#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
typedef long long ll;
 
std::string print(std::vector<int>& vec) {
    std::string res;
    for (int p : vec) {
        res += std::to_string(vec[p]) + " ";
    }
    return res;
}
 
std::vector<int>& parse(std::string input) {
    std::string dop;
    std::vector<int> numbers;
    for (int i = 0; i < input.size(); i++) {
        if (input[i] == '=' || input[i] == '+') {
            numbers.push_back(std::stoi(dop));
            dop = "";
        }
        else dop += input[i];
    }
    if (dop.size()) numbers.push_back(std::stoi(dop));
    return numbers;
}
 
std::string num(int n, int k) {
    int d = 1;
    std::string res;
    std::vector<std::vector<long long>> dop(n, std::vector<long long>(n, 0));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) dop[i][j] = 1;
            else {
                for (int k = j; k <= i; k++) dop[j][i] += dop[k][i - j - 1];
            }
        }
    }
 
    while (n != d) {
        if (dop[d - 1][n - 1] > k) {
            res += std::to_string(d) + "+";
            n -= d;
        }
        else k -= dop[++d - 2][n - 1];
    }
    res += std::to_string(d);
    return res;
}
 
int main() {
    int n, k;
    std::cin >> n >> k;
    std::cout << num(n, k);
    return 0;
}