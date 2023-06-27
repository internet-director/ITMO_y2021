#include <iostream>
#include <functional>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>

bool is_cycle = false;

int check(std::vector<std::set<int>>& matrix, int i, int j) {
    return matrix[i].find(j) != matrix[i].end();
}

void init(std::vector<std::set<int>>& matrix, int i, int j, bool set = true) {
    matrix[i].insert(j);
}


void dfs(std::vector<std::set<int>>& mat, std::vector<int>& used, std::deque<int>& sort, int v) {
    used[v] = 1;

    for (auto i : mat[v]) {
        if (!used[i]) {
            dfs(mat, used, sort, i);
        }
        else if (used[i] == 1) {
            is_cycle = true;
            return;
        }
    }
    used[v] = -1;
    sort.push_front(v);
}


int main() {
    int n, m;
    std::cin >> n >> m;
    std::deque<int> res;
    std::vector<int> used(n + 1, 0);
    std::vector<std::set<int>> vec(n + 1);

    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        init(vec, a, b);
    }

    for (int i = 0; i < n; i++) {
        if (!used[i + 1]) {
            dfs(vec, used, res, i + 1);
            if (is_cycle) {
                std::cout << -1;
                return 0;
            }
        }
    }
    for (int i = 0; i < n; i++) {
        std::cout << res[i] << " ";
    }

    return 0;
}