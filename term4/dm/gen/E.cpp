#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <type_traits>

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
    std::vector<std::int64_t> vec(d + 1);

    for (int i = 0; i <= d; i++) {
        std::cin >> vec[i];
    }

    std::vector<std::int64_t> Q(2), dop;

    Q[0] = 1;
    Q[1] = -r;

    dop.emplace_back(1);
    dop.emplace_back(-r);


    for (int i = 0; i < d; i++) {
        mulVector(dop, Q, Q);
    }

    while (!Q[Q.size() - 1]) Q.pop_back();

    std::vector<std::int64_t> P(d + 1);

    {
        std::int64_t pw = 1;
        for (std::int64_t i = 0; i <= d; i++) {
            std::int64_t step = 1;
            for (int j = 0; j <= d; j++) {
                P[i] += step * vec[j] * pw;
                step *= i;
            }
            pw *= r;
        }

        mulVector(P, Q, P);
    }

    while(P.size() > d + 1) P.pop_back();

    while (!P[P.size() - 1]) P.pop_back();

    std::cout << P.size() - 1 << std::endl;

    for (auto i: P) std::cout << i << " ";
    std::cout << std::endl << Q.size() - 1 << std::endl;
    for (auto i: Q) std::cout << i << " ";

    return 0;
}