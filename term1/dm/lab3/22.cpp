#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
int main() {
    unsigned long long result = 0, sum = 0, last = 0;
    std::string input, d;
    std::cin >> input;
    int n = input.size(), tmp = 0;
    std::vector<int> numbers;
 
    for (int i = 0; i < n; i++) {
        if (input[i] == '+') {
            numbers.push_back(std::stoi(d));
            d = "";
            tmp += numbers[numbers.size() - 1];
        }
        else d += input[i];
    }
    if (d.size()) {
        numbers.push_back(std::stoi(d));
        d = "";
        tmp += numbers[numbers.size() - 1];
    }
 
    n = tmp;
    std::vector<std::vector<long long>> dop(n, std::vector<long long>(n, 0));
 
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) dop[i][j] = 1;
            else {
                for(int k = j; k <= i; k++) dop[j][i] += dop[k][i - j - 1];
            }
        }
    }
 
    for (int i = 0; i < numbers.size(); i++) {
        for (last; last < numbers[i] - 1; last++) result += dop[last][n - 1];
        n -= last + 1;
    }
 
    std::cout << result;
    return 0;
}