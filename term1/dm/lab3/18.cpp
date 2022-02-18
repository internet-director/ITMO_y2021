#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
int main() {
    unsigned long long n, k, check = 1;
    std::string result;
    std::cin >> n >> k;
    n <<= 1;
    std::vector<std::vector<long long>> dop(n + 1, std::vector<long long>(n + 1, 0));
    dop[0][0] = 1;
    for (int i = 1; i <= n; i++) {
        dop[i][0] += dop[i - 1][1];
        for (int j = 1; j < n; j++) {
            dop[i][j] += dop[i - 1][j - 1];
            dop[i][j] += dop[i - 1][j + 1];
        }
    }
    for (int i = 1; i < n; i++) dop[n][i] += dop[n - 1][i - 1];
 
    for (int i = 0; i < n; i++) {
        if (dop[n - i - 1][check] > k) {
            result += '(';
            check++;
        }
        else {
            k -= dop[n - i - 1][check--];
            result += ')';
        }
    }
 
    std::cout << result;
    return 0;
}