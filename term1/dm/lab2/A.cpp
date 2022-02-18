
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <Windows.h>
#include <cmath>
#include <bitset>

int main() {
    unsigned long long n, count = 0;
    std::cin >> n;

    std::vector< unsigned long long> vec(n), b;
    for (int i = 0; i < n; i++) {
        std::cin >> vec[i];
    }

    std::sort(vec.begin(), vec.end());

    while (vec.size() > 1) {
        unsigned long long sum = vec[0] + vec[1];
        vec.erase(vec.begin());
        vec.erase(vec.begin());

        vec.insert(vec.begin(), sum);
        b.push_back(sum);
        std::sort(vec.begin(), vec.end());
    }

    for (int i = 0; i < b.size(); i++) count += b[i];

    std::cout << count;
    return 0;
}
