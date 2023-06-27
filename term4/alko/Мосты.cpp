#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>
#include <map>

namespace std {
    template<typename T1, typename T2, typename T3>
    struct triple {
        T1 a;
        T2 b;
        T3 c;

        triple() = default;
        triple(const T1& a, const T2& b, const T3& c) : a(a), b(b), c(c) { }
        triple(T1& a, T2&& b, T3&& c) : a(std::move(a)), b(std::move(b)), c(std::move(c)) { }
    };
}

struct comp {
    bool operator()(const std::triple<int, int, int>& a, const std::triple<int, int, int>& b) const {
        return std::tie(a.a, b.b) < std::tie(b.a, a.b);
    }
};

int timer = 0;

bool check(std::vector<std::set<int>>& matrix, int i, int j) {
    return matrix[i].find(j) != matrix[i].end();
}

void init(std::vector<std::set<int>>& matrix, int i, int j, bool val = true) {
    matrix[i].insert(j);
    matrix[j].insert(i);
}

void dfs(std::vector<std::set<int>>& mat, std::vector<int>& used, std::vector<int>& tin, std::vector<int>& dop, std::set<std::triple<int, int, int>, comp>& re_dop, std::vector<int>& result, int v, int prev) {
    used[v] = 1;
    tin[v] = timer;
    dop[v] = timer;
    timer++;
    for (auto& i : mat[v]) {
        bool us = (used[i] == 0);
        if (i == prev) continue;
        if (us) {
            dfs(mat, used, tin, dop, re_dop, result, i, v);
            if (dop[v] > dop[i]) dop[v] = dop[i];
            if (dop[i] > tin[v]) {
                auto it = re_dop.find({ i, v, 0 });
                if (it == re_dop.end())
                    it = re_dop.find({ v, i, 0 });
                result.push_back(it->c);
            }
        }
        else {
            if (dop[v] > tin[i]) dop[v] = tin[i];
        }
    }
}

int main() {
    int n, m;
    std::cin >> n >> m;


    std::vector<int> used(n + 1), tin(n + 1), dop(n + 1), result;
    std::set<std::triple<int, int, int>, comp> re_dop;
    std::vector<std::set<int>> mat(n + 1);

    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        init(mat, a, b);
        re_dop.insert({ a, b, i + 1 });
    }


    for (int i = 0; i < n; i++) {
        if (used[i + 1] == 0) {
            dfs(mat, used, tin, dop, re_dop, result, i + 1, 0);
        }
    }

    std::cout << result.size() << "\n";
    std::sort(result.begin(), result.end());
    for (auto i : result) {
        std::cout << i << "\n";
    }

    return 0;
}