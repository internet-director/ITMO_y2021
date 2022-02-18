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
    ll n, result = 0;
    std::cin >> n;
    std::vector<long long> numbers(n), f(n, 1), dop(n, 0);
    for (int i = 0; i < n; i++) std::cin >> numbers[i];
    for (int i = 1; i < n; i++) f[i] = f[i - 1] * (i + 1);
 
    //...
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < numbers[i] - 1; j++) {
            if (!dop[j]) result += f[n - i - 2];
        }
        dop[numbers[i] - 1] = 1;
    }
    std::cout << result;
    return 0;
}