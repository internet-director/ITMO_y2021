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

int main() {
    ll n, k;
    std::cin >> n >> k;
    std::string result;
    std::vector<long long> numbers;
    for (int i = 0; i < n; i++) numbers.push_back(i + 1);
    ll f = fact(n - 1);


    for (int i = 0; i < n; i++) {
        ll col = k / f;
        k %= f;
        ll dop = (n - i - 1);
        if (!dop) dop = 1;
        f /= dop;

        for (ll j = 0; j < numbers.size(); j++) {
            if (col == j) {
                result += std::to_string(numbers[j]) + " ";
                numbers.erase(numbers.begin() + j);
            }
        }
    }
    std::cout << result;
    return 0;
}