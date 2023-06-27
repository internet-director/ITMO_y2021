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

int maxc = 0;

int timer = 0;

bool check(std::vector<std::set<int>>& matrix, int i, int j) {
    return matrix[i].find(j) != matrix[i].end();
}

void init(std::vector<std::set<int>>& matrix, int i, int j, bool val = true) {
    matrix[i].insert(j);
    matrix[j].insert(i);
}

void dfs(std::vector<std::set<int>>& mat, std::vector<int>& used, std::vector<int>& tin, std::vector<int>& dop, std::set<std::triple<int, int, int>, comp>& re_dop, std::vector<std::pair<int, int>>& result, int v, int prev) {
    used[v] = 1;
    timer++;
    tin[v] = timer;
    dop[v] = timer;
    for (auto& i : mat[v]) {
        bool us = (used[i] == 0);
        if (i == prev) continue;
        if (us) {
            dfs(mat, used, tin, dop, re_dop, result, i, v);
            if (dop[v] > dop[i]) dop[v] = dop[i];
            if (dop[i] > tin[v]) {
                result.push_back({ i, v });
            }
        }
        else {
            if (dop[v] > tin[i]) dop[v] = tin[i];
        }
    }
}

void dfs(std::vector<std::set<int>>& mat, std::map<std::pair<int, int>, int>& kostil, std::vector<int>& used, std::vector<int>& tin, std::vector<int>& dop, std::vector<std::pair<int, int>>& result, int color, int v) {
    used[v] = color;
    for (auto i : mat[v]) {
        if (used[i] == 0) {
            if (dop[i] > tin[v] && kostil[std::pair<int, int>(i, v)] == 1)
                dfs(mat, kostil, used, tin, dop, result, ++maxc, i);
            else dfs(mat, kostil, used, tin, dop, result, color, i);
        }
    }
}

int main() {
    int n, m;
    std::cin >> n >> m;


    std::vector<int> used(n + 1), tin(n + 1), dop(n + 1);
    std::vector<std::pair<int, int>> result;
    std::map<std::pair<int, int>, int> kostil;
    std::set<std::triple<int, int, int>, comp> re_dop;
    std::vector<std::set<int>> mat(n + 1);

    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        init(mat, a, b);
        kostil[std::pair<int, int>(a, b)]++;
        kostil[std::pair<int, int>(b, a)]++;
    }


    for (int i = 0; i < n; i++) {
        if (used[i + 1] == 0) {
            dfs(mat, used, tin, dop, re_dop, result, i + 1, 0);
        }
    }

    std::fill(used.begin(), used.end(), 0);

    for (int i = 0; i < n; i++) {
        if (used[i + 1] == 0) {
            dfs(mat, kostil, used, tin, dop, result, ++maxc, i + 1);
        }
    }

    std::cout << maxc << "\n";
    for (int i = 0; i < n; i++) {
        std::cout << used[i + 1] << " ";
    }

    return 0;
}