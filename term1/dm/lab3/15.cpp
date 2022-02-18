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
    ll n, k, count, d = 0;
    std::string result;
    std::cin >> n >> k >> count;
    std::vector<long long> numbers(n);
    std::vector<std::vector<long long>> dop(n + 1, std::vector<long long>(k + 1, 1));
    for (int i = 0; i < n; i++) numbers[i] = i + 1;
    for (int i = 0; i <= n; i++) {
        for (int j = 1; j <= k; j++) {
            if (i > j) dop[i][j] = dop[i][j - 1] * (i - j + 1) / j;
            else if (i < j) dop[i][j] = 0;
        }
    }
    while (k > 0) {
        d++;
        n--;
        if (count < dop[n][k - 1]) {
            result += std::to_string(d) + " ";
            k--;
        }
        else {
            count -= dop[n][k - 1];
        }
    }

    std::cout << result;
    return 0;
}