#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <map>

#define MOD 1000000007

std::int64_t calc(int n, std::vector<std::int64_t>& res, std::vector<int>& vec, std::map<std::int64_t, std::int64_t>& mapa)
{
    if(n == 0) return 1;
    else if(res[n] != INT64_MAX) return res[n];
    res[n] = 0;
    for(auto c : vec) {
        std::int64_t k = 0;
        if(mapa.count(n - c)) k = mapa[n - c];
        else {
            for(int i = 0; i <= n - c; i++) {
                k = (k + calc(i, res, vec, mapa) * calc(n - c - i, res, vec, mapa) % MOD) % MOD;
            }
            mapa[n - c] = k;
        }

        res[n] = (res[n] + k) % MOD;
    }
    return res[n];
}

int main() {
    int k, m;
    std::cin >> k >> m;
    std::vector<int> vec(k);
    for (int i = 0; i < k; i++) std::cin >> vec[i];

    std::vector<std::int64_t> ult(m + 1, INT64_MAX);
    std::map<std::int64_t, std::int64_t> mapa;
    ult[0] = 1;
    for(int i = 1; i <= m; i++) {
        calc(i, ult, vec, mapa) % MOD;
    }
    for(int i = 1; i < ult.size(); i++) std::cout << ult[i] << " ";

    return 0;
}