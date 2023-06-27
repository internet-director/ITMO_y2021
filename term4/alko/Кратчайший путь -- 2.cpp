#include <iostream>
#include <fstream>
#include <memory>
#include <cstring>
#include <string_view>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>
#include <variant>
#include <map>

namespace std {
    template<typename T1, typename T2, typename T3>
    struct triple {
        triple() = default;
        triple(const T1& t1, const T2& t2, const T3& t3) :a(t1), b(t2), c(t3) {}
        triple(T1& t1, T2& t2, T3& t3) :a(std::move(t1)), b(std::move(t2)), c(std::move(t3)) {}

        T1 a;
        T2 b;
        T3 c;
    };
}

struct comp {
    bool operator()(const std::triple<int, int, int>& a, const std::triple<int, int, int>& b) const {
        return std::tie(a.b, b.c) < std::tie(b.b, a.c);
    }
};

using triple = std::triple<int, int, int>;

int main(int argc, char** argv) {
    std::ios_base::sync_with_stdio(false);
    std::uint64_t n, m;
    std::cin >> n >> m;
    std::set<triple, comp> res;
    std::vector<bool> used(n, false);
    std::vector<int> d(n, INT32_MAX);
    std::vector<std::vector<triple>> matrix(n);

    for (int i = 0; i < m; i++) {
        int a, b, c;
        std::cin >> a >> b >> c;
        a--;
        b--;
        matrix[a].push_back(triple(0, b, c));
        matrix[b].push_back(triple(0, a, c));
    }

    d[0] = 0;
    res.insert(triple(0, 0, 0));


    while (!res.empty()) {
        int v = res.begin()->b;
        res.erase(res.begin());
        for (auto& it : matrix[v]) {
            if (d[it.b] > d[v] + it.c) {
                res.erase(triple(0, it.b, d[it.b]));
                d[it.b] = d[v] + it.c;
                res.insert(triple(0, it.b, d[it.b]));
            }
        }
    }

    for (int i = 0; i < n; i++) {
        std::cout << d[i] << " ";
    }

    return 0;
}