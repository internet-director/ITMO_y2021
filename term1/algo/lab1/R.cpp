#include <iostream>
#include <algorithm>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>
#include <map>

typedef unsigned long long ull;

ull n, k;

bool test(ull m, std::vector<ull>& vec) {
    ull sum = 0, count = 0;
    for (ull i = 0; i < vec.size(); i++) {
        sum += vec[i];
        if (vec[i] > m) return false;
        if (sum > m) {
            sum = vec[i];
            count++;
        }
    }
    if (count < k) return true;
    return false;
}

int main() {
    ull result = 0, m = 0, sum = 0;
    std::cin >> n >> k;
    std::vector<ull> vec(n);
    for (ull i = 0; i < n; i++) std::cin >> vec[i];

    for (ull i = 0; i < vec.size(); i++) {
        sum += vec[i];
        if (m < vec[i]) m = vec[i];
    }

    while (m <= sum) {
        ull d = (m + sum) / 2;
        if (test(d, vec)) {
            sum = d - 1;
            result = d;
        }
        else m = d + 1;
    }

    std::cout << result;
    return 0;
}