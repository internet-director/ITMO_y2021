#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

#define MOD 998244353
typedef std::int64_t int_t;
typedef std::uint64_t uint_t;

std::int64_t gcdex(std::int64_t a, std::int64_t b, std::int64_t& x, std::int64_t& y) {
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

uint_t c(int_t n, int_t k) {
    if (n < 0) n = 0;
    if (n < k) return 0;
    uint_t res = 1, div = 1;
    for (uint_t i = n - k + 1; i <= n; i++) res = (res * i) % MOD;
    for (uint_t i = 2; i <= k; i++) div = (div * i) % MOD;

    int_t x = 0, y = 0;
    gcdex(div, MOD, x, y);
    return (res * (x + MOD)) % MOD;
}

int main() {
    int n, k;
    std::cin >> k >> n;
    std::vector<uint_t> p(n), q(n);

    {
        for (uint_t i = 0; i < p.size(); i++) {
            p[i] = ((1 - 2 * (i % 2)) * c(k - i - 2, i) + MOD) % MOD;
        }

        for (uint_t i = 0; i < q.size(); i++) {
            q[i] = ((1 - 2 * (i % 2)) * c(k - i - 1, i) + MOD) % MOD;
        }
    }

    {
        std::vector<long long> A(n);

        A[0] = p[0];

        for (int i = 1; i < n; i++) {
            uint_t tmp = 0;
            for (int j = 1; j <= i; j++) {
                tmp = (MOD + tmp + (A[i - j] * q[j]) % MOD) % MOD;
            }
            A[i] = (MOD + p[i] - tmp) % MOD;
        }

        for (int i = 0; i < n; i++) {
            std::cout << A[i] << "\n";
        }
    }

    return 0;
}