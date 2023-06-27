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
}

void dfs(std::vector<std::set<int>>& mat, std::vector<int>& used, std::vector<int>& result, int v, int c, bool t = true) {
    used[v] = c;
    for (auto i : mat[v]) {
        if (used[i] == 0)
            dfs(mat, used, result, i, c, t);
    }
    if (t) result.push_back(v);
}

int32_t main() {
    int n, m;
    std::cin >> n >> m;


    std::vector<int> used(n + 1), res1;
    std::set<int> result;
    std::set<std::triple<int, int, int>, comp> re_dop;
    std::vector<std::set<int>> mat1(n + 1), mat2(n + 1);

    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        init(mat1, a, b);
        init(mat2, b, a);
        re_dop.insert({ a, b, i + 1 });
    }

    for (int i = 0; i < n; i++) {
        if (used[i + 1] == 0) {
            dfs(mat1, used, res1, i + 1, 1);
        }
    }
    std::fill(used.begin(), used.end(), 0);
    std::reverse(res1.begin(), res1.end());
    int c = 1;
    for (int i = 0; i < n; i++) {
        if (used[res1[i]] == 0) {
            std::vector<int> res2;
            dfs(mat2, used, res2, res1[i], c, false);
            c++;
        }
    }
    int sz = 0;
    std::set<std::triple<int, int, int>, comp> res;
    for (auto it = re_dop.begin(); it != re_dop.end(); it++) {
        if (used[it->a] != used[it->b]) {
            res.insert({std::min(used[it->a], used[it->b]), std::max(used[it->a], used[it->b]), 0});
        }
    }

    std::cout << res.size() << "\n";
    return 0;
}