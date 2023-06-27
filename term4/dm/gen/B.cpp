#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

#define MOD 998244353

namespace std {
    template<typename T1, typename T2, typename T3>
    struct triple {
        T1 a;
        T2 b;
        T3 c;

        triple() = default;

        template<typename F1, typename F2, typename F3>
        triple(F1 &&f1, F2 &&f2, F3 &&f3) : a(std::forward<T1>(f1)),
                                            b(std::forward<T2>(b)),
                                            c(std::forward<T3>(f3)) {

        }
    };

    template<typename T>
    void mulVector(std::vector<T> &vec1, std::vector<T> &vec2, std::vector<T> &res) {
        std::vector<T> result(vec1.size() + vec2.size());
        for (int i = 0; i < vec1.size(); i++) {
            for (int j = 0; j < vec2.size(); j++) {
                result[i + j] =
                        (result[i + j] + MOD + (((vec1[i] * vec2[j] + MOD) % MOD) * (1 - 2 * (j % 2)) + MOD) % MOD) %
                        MOD;
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

        std::int64_t x1 = 0, y1 = 0, d = gcdex(b % a, a, x1, y1);
        x = ((y1 - (b / a * x1) % MOD) % MOD + MOD) % MOD;
        y = (x1 + MOD) % MOD;
        return d;
    }

    template<typename T>
    void mulVector2(std::vector<T> &vec1, std::vector<T> &vec2, int n, std::vector<T> &res) {
        std::vector<T> result(n);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j <= i; j++) {
                if (j >= vec1.size()) continue;
                if (i - j >= vec2.size()) continue;
                result[i] = (result[i] + MOD + (vec1[j] * vec2[i - j]) % MOD) % MOD;
            }
        }
        res = std::move(result);
    }

    void sumVector(std::vector<std::int64_t> &vec1, std::vector<std::int64_t> &vec2, std::vector<std::int64_t> &res) {
        std::vector<std::int64_t> result(std::max(vec1.size(), vec2.size()));
        for (int i = 0; i < result.size(); i++) {
            if (i < vec1.size()) result[i] = (result[i] + MOD + vec1[i]) % MOD;
            if (i < vec2.size()) result[i] = (result[i] + MOD + vec2[i]) % MOD;
        }
        res = std::move(result);
    }

    std::int64_t fuck(std::int64_t num) {
        if (num == 0 || num == 1) return 1;
        return (fuck(num - 1) * num) % MOD;
    }
}

void gcdDV(std::vector<std::int64_t> &vec, std::int64_t num, std::vector<std::int64_t> &res) {
    std::vector<std::int64_t> result(vec.size());
    for (int i = 0; i < result.size(); i++) {
        std::int64_t x = 0, y = 0;
        std::gcdex(num, MOD, x, y);
        result[i] = (vec[i] * (x + MOD) % MOD + MOD) % MOD;
    }
    res = std::move(result);
}

int main() {
    int n, m;
    std::cin >> n >> m;
    std::vector<std::int64_t> vec(n + 1), dop(n + 1), kostil;
    for (int i = 0; i <= n; i++) std::cin >> vec[i];

    {
        kostil.resize(1, 1);
        std::vector<std::int64_t> rE(1), rLN(1), rSQRT, kostilLN(1, 1), kostilSQRT(1, 1);
        std::int64_t f = 1, step = 1;
        for (int i = 0; i < m; i++) {
            if (i) f = (f * i) % MOD;
            std::vector<std::int64_t> tmp;
            gcdDV(kostil, f, tmp);
            std::sumVector(rE, tmp, rE);
            std::mulVector2(kostil, vec, m, kostil);

            tmp.clear();
            if (i) {
                gcdDV(kostilLN, i, tmp);
                for (int j = 0; j < tmp.size() && (i % 2 - 1); j++) {
                    tmp[j] = (-tmp[j] + MOD) % MOD;
                }
            }

            std::sumVector(rLN, tmp, rLN);
            std::mulVector2(kostilLN, vec, m, kostilLN);

            std::int64_t ans = ((((((step * f + MOD) % MOD) * f + MOD) % MOD + MOD) * (1 - 2 * i)) % MOD + MOD) % MOD;
            std::vector<std::int64_t> l(1, ((std::fuck(i << 1) * (1 - 2 * (i % 2) + MOD)) % MOD));
            std::mulVector(kostilSQRT, l, l);
            gcdDV(l, ans, l);

            std::sumVector(rSQRT, l, rSQRT);
            std::mulVector2(kostilSQRT, vec, m, kostilSQRT);

            step = (step * 4) % MOD;
        }
        for (int i = 0; i < m; i++) std::cout << rSQRT[i] << " ";
        std::cout << std::endl;
        for (int i = 0; i < m; i++) std::cout << rE[i] << " ";
        std::cout << std::endl;
        for (int i = 0; i < m; i++) std::cout << rLN[i] << " ";
    }

    return 0;
}