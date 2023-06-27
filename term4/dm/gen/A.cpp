#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

#define MOD 998244353

int main() {
    int n, m;
    std::cin >> n >> m;
    std::vector<long long> p(std::max(n, m) + 1), q(std::max(n, m) + 1);
    for(int i = 0; i <= n; i++) {
        std::cin >> p[i];
    }

    for(int i = 0; i <= m; i++) {
        std::cin >> q[i];
    }


    {
        std::cout << std::max(n, m) << "\n";

        for(int i = 0; i <= std::min(n, m); i++) {
            std::cout << (p[i] + q[i]) % MOD << " ";
        }
        for(int i = std::min(n, m) + 1; i <= std::max(n, m); i++) {
            if(n > m) std::cout << p[i] << " ";
            else std::cout << q[i] << " ";
        }
    }

    {
        std::cout << "\n" << n + m << "\n";
        std::vector<long long> M(n + m + 1);
        for(int i = 0; i <= n; i++) {
            for(int j = 0; j <= m; j++) {
                M[i + j] = (MOD + M[i + j] + (p[i] * q[j] % MOD)) % MOD;
            }
        }
        for(auto i: M) {
            std::cout << i << " ";
        }
    }

    {
        std::cout << "\n";
        std::vector<long long> A(1001);

        A[0] = p[0];

        for(int i = 1; i < p.size(); i++) {
            long long tmp = 0;
            for(int j = 0; j < i; j++) {
                tmp = (MOD + tmp + (A[j] * q[i - j]) % MOD) % MOD;
            }
            A[i] = (MOD + p[i] - tmp) % MOD;
        }

        for(int i = p.size(); i < 1000; i++) {
            for(int j = 1; j <= m; j++) {
                A[i] = (MOD + A[i] -(q[j] * A[i - j]) % MOD) % MOD;
            }
        }

        for(int i = 0; i < 1000; i++) {
            std::cout << A[i] << " ";
        }
    }

    return 0;
}
