#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
 
typedef long long ll;
 
ll fact(ll i) {
    if (i <= 1) return 1;
    return i * fact(i - 1);
}
ll min(ll a, ll b) {
    return (a > b) ? b : a;
}
 
int main() {
    ll n, k, result = 0;
    std::cin >> n >> k;
    std::vector<long long> numbers(k + 1, 0);
    std::vector<std::vector<long long>> dop(n + 1, std::vector<long long>(k + 1, 1));
    for (int i = 0; i < k; i++) std::cin >> numbers[i + 1];
 
    for (int i = 0; i <= n; i++) {
        for (int j = 1; j <= k; j++) {
            if (i > j) dop[i][j] = dop[i][j - 1] * (i - j + 1) / j;
            else dop[i][j] = 0;
        }
    }
 
    for (int i = 1; i <= k; i++) {
        for (int j = numbers[i - 1] + 1; j < numbers[i]; j++) {
            result += dop[n - j][k - i];
        }
    }
 
    std::cout << result;
    return 0;
}