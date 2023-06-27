#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <cmath>
#include <numeric>
#include <deque>

#define MOD 104857601

template<typename T>
void mulVector(std::vector<T> &vec1, std::vector<T> &vec2, std::vector<T> &res) {
    std::vector<T> result(vec1.size() + vec2.size());
    for (int i = 0; i < vec1.size(); i++) {
        for (int j = 0; j < vec2.size(); j++) {
            result[i + j] = (result[i + j] + MOD + (vec1[i] * vec2[j] * (1 - 2 * (j % 2))) % MOD) % MOD;
        }
    }
    res = std::move(result);
}

std::int64_t gcdex(std::int64_t a, std::int64_t b, std::int64_t &x, std::int64_t &y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return b;
    }
    std::int64_t x1, y1, d = gcdex(b % a, a, x1, y1);
    x = (y1 - ((b / a) * x1));
    y = x1;
    return d;
}

int main() {
    std::int64_t k, n;

    std::cin >> k >> n;

    std::vector<std::int64_t> c(k), a(k), dop(k + 1), kostil(k);
    dop[0] = 1;
    for (int i = 0; i < k; i++) {
        std::cin >> a[i];
        kostil[i] = a[i];
    }
    for (int i = 0; i < k; i++) {
        std::cin >> c[i];
        dop[i + 1] -= c[i];
    }

    for (int i = 0; i < k; i++) {
        for (int j = 0; j < i; j++) {
            kostil[i] = (kostil[i] + MOD - c[j] * a[i - j - 1] % MOD) % MOD;
        }
    }

    for (std::int64_t i = n - 1; i > 0; i /= 2) {
        mulVector(kostil, dop, kostil);
        mulVector(dop, dop, dop);

        for (int j = 0; j < std::max(kostil.size(), dop.size()); j += 2) {
            if (j < kostil.size()) kostil[j / 2] = kostil[j + i % 2];
            if (j < dop.size()) dop[j / 2] = dop[j];
        }

        dop.resize(dop.size() / 2);
        kostil.resize(kostil.size() / 2);
    }


    std::cout << kostil[0];
    return 0;
}