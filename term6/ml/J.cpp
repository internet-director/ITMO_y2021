#define _CRT_DISABLE_PERFCRIT_LOCKS

#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <iomanip>
#include <cmath>
#include <type_traits>
#include <cstdio>
#include <cstdint>
#include <utility>

namespace std {
    template <typename T1, typename T2, typename T3>
    struct triple {
        constexpr explicit triple() {

        }
        constexpr explicit triple(T1&& a, T2&& b, T3&& c) {
            this->a = std::forward<T1>(a);
            this->b = std::forward<T2>(b);
            this->c = std::forward<T3>(c);
        }

        T1 first;
        T2 middle;
        T3 last;
    };
}

struct my_hash {
    template <class T1, class T2>
    size_t operator()(const std::pair<T1, T2>& p) const
    {
        const auto hash1 = std::hash<T1>{}(p.first);
        const auto hash2 = std::hash<T2>{}(p.second);

        if (hash1 != hash2) {
            return hash1 ^ hash2;
        }
        return hash1;
    }
};

using upair = std::pair<std::int64_t, std::int64_t>;
using dpair = std::pair<std::double_t, std::double_t>;
using utriple = std::triple<std::int64_t, std::int64_t, std::int64_t>;

double divs_prepa(int n, const std::unordered_map<std::int64_t, std::int64_t>& mapa) {
    double res = 0.0;
    for (const auto& [v, key] : mapa)
        res += double(v) * double(key) / double(n);
    return res;
}

double disp(int n, const std::unordered_map<std::int64_t, std::int64_t>& mapa) {
    double res = 0.0;
    double tmp = divs_prepa(n, mapa);

    for (const auto& [v, key] : mapa)
        res += std::pow(v - tmp, 2.0) * double(key) / double(n);

    return res;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    int k, n;
    std::cin >> k >> n;

    std::unordered_map<std::int64_t, std::vector<std::int64_t>> mapa;
    std::unordered_map<std::int64_t, std::int64_t> m1, m2, dop;
    std::vector<dpair> data;

    for (int i = 0; i < n; i++) {
        std::int64_t x, y;
        std::cin >> x >> y;
        m1[x]++;
        m2[y]++;
        mapa[x].emplace_back(y);
    }

    for (const auto& [v, key] : m1) {
        dop.clear();
        for (const auto& it : mapa[v]) dop[it]++;
        data.emplace_back(
            divs_prepa(mapa[v].size(), dop),  
            double(key) / double(n)
        );
    }

    const auto res1 = std::accumulate(data.begin(), data.end(), 0.0,
        [&](double acc, const dpair& it) {
            return acc + it.second * it.first;
        });

    const auto res2 = std::accumulate(data.begin(), data.end(), 0.0,
        [&res1](double acc, const dpair& it) {
            return acc + it.second * std::pow(it.first - res1, 2.0);
        });

    std::cout << std::setprecision(12) 
        << double(disp(n, m2) - res2) 
        << std::endl;

    return 0;
}