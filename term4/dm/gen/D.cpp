#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <cmath>
#include <numeric>

void mulVector(std::vector<std::int64_t> &vec1, std::vector<std::int64_t> &vec2, std::vector<std::int64_t> &res) {
    std::vector<std::int64_t> result(vec1.size() + vec2.size());
    for (int i = 0; i < vec1.size(); i++) {
        for (int j = 0; j < vec2.size(); j++) {
            result[i + j] += vec1[i] * vec2[j];
        }
    }
    res = std::move(result);
}

int main() {
    int r, d;
    std::cin >> r >> d;
    std::vector<std::int64_t> vec(d + 1), kostil(2 * d + 1);

    std::int64_t pow = 1;
    for (int i = 0; i <= d; i++) {
        std::cin >> vec[i];
        if (i != 0) pow *= r;
    }

    std::vector<std::int64_t> Q(1), dop;

    dop.emplace_back(1);
    dop.emplace_back(1);

    for (int j = 0; j <= d; j++, pow /= r) {
        Q[0] = pow * vec[j];
        for (int i = 0; i < d; i++) {
            dop[1] = i - j + 1;
            mulVector(dop, Q, Q);
        }
        for (int i = 0; i < Q.size(); i++) {
            kostil[i] += Q[i];
        }
        Q.resize(1);
    }

    std::int64_t fuck = 1;
    for (int i = 1; i <= d; i++) {
        fuck *= r * i;
    }

    for (int i = d; i >= 0; i--) {
        std::int64_t gcd = std::gcd(fuck, kostil[i]);
        std::cout << kostil[i] / gcd << "/" << fuck / gcd << " ";
    }

    return 0;
}