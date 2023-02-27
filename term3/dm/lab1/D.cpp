#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <stack>

int check(std::vector<std::string>& matrix, int i, int j) {
    bool fix = false;
    if (i < j) {
        std::swap(i, j);
        fix = true;
    }
    return matrix[i][j] == '1' ^ fix;
}

void dfs(std::vector<std::string>& mat, std::vector<bool>& used, std::deque<int>& sort, int v) {
    used[v] = true;

    for (int i = 0; i < used.size(); i++) {
        if (!used[i]) {
            if (check(mat, v, i)) {
                dfs(mat, used, sort, i);
            }
        }
    }

    sort.push_back(v);
}

int main() {
    int n;
    std::cin >> n;

    std::vector<std::string> mat(n);


    for (int i = 0; i < n - 1; i++) {
        std::cin >> mat[i + 1];
    }


    for (int i = 0; i < n; i++) {
        std::deque<int> queue;
        std::vector<bool> used(n, 0);
        dfs(mat, used, queue, i);
        if(check(mat, queue.front(), queue.back())) {
            std::reverse(queue.begin(), queue.end());
            for (auto i : queue) {
                std::cout << i + 1 << " ";
            }
            break;
        }
    }

    return 0;
}