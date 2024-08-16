#define _CRT_DISABLE_PERFCRIT_LOCKS

#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <type_traits>
#include <cstdio>
#include <cstdint>

namespace std {
    template <typename T1, typename T2, typename T3>
    struct triple {
        constexpr explicit triple() {

        }
        constexpr explicit triple(const T1& a, const T2& b, const T3& c) {
            this->a = a;
            this->b = b;
            this->c = c;
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
        auto hash1 = std::hash<T1>{}(p.first);
        auto hash2 = std::hash<T2>{}(p.second);

        if (hash1 != hash2) {
            return hash1 ^ hash2;
        }
        return hash1;
    }
};

using upair = std::pair<std::int64_t, std::int64_t>;
using utriple = std::triple<std::int64_t, std::int64_t, std::int64_t>;

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::int64_t dist = 0;
    std::int64_t prefix = 0;
    std::int64_t suffix = 0;
    std::uint64_t k, n;
    std::cin >> k >> n;

    std::unordered_map<std::int64_t, std::int64_t> siffix_sz, prefix_sz, kostil1, kosilt2;
    std::unordered_map<std::int64_t, std::vector<std::int64_t>> mapa;
    std::vector<upair> data;

    for (int i = 0; i < n; i++) {
        int x, y;
        std::cin >> x >> y;
        mapa[y].emplace_back(x);
        data.emplace_back(x, y);
    }

    for (auto& [key, val] : mapa) {
        std::sort(val.begin(), val.end());

        std::int64_t prefix = 0;
        std::int64_t suffix = std::accumulate(val.begin(), val.end(), 0);

        std::size_t cnt = 0;

        for (const auto& it : val) {
            cnt++, suffix -= it, prefix += it;
            dist += it * cnt - prefix + suffix - it * (val.size() - cnt);
        }
    }

    prefix = suffix = 0;

    std::cout << dist << "\n";

    std::sort(data.begin(), data.end(), [](const upair& a, const upair& b) {
        return a.first == b.first ? a.second <= b.second : a.first <= b.first;
        });

    for (const auto& it : data) {
        siffix_sz[it.second]++;
        kostil1[it.second] += it.first;
        suffix += it.first;
    }

    dist = 0;
    size_t cnt = 0;

    for (const auto& it : data) {
        cnt++;
        siffix_sz[it.second]--;
        prefix_sz[it.second]++;

        prefix += it.first;
        suffix -= it.first;

        kostil1[it.second] -= it.first;
        kosilt2[it.second] += it.first;

        dist += (cnt - prefix_sz[it.second]) * it.first - prefix + kosilt2[it.second];
        dist += suffix - kostil1[it.second];
        dist -= (data.size() - cnt - siffix_sz[it.second]) * it.first;
    }

    std::cout << dist << "\n";

    return 0;
}
