#include <iostream>
#include <functional>
#include <vector>
#include <string>
#include <queue>
#include <stack>
#include <set>

struct meta {
    int num = 0;
    int deg = 0;

    meta() { }
    meta(int n) : num(n) { }

    struct comp {
        bool operator()(const meta& a, const meta& b) {
            return a.deg < b.deg;
        }
    };
};

int check(std::vector<std::set<int>>& matrix, int i, int j) {
    return matrix[i].find(j) != matrix[i].end();
}

void init(std::vector<std::set<int>>& matrix, int i, int j, bool set = true) {
    matrix[i].insert(j);
    matrix[j].insert(i);
}

int main() {
    int n, m;
    std::cin >> n >> m;
    std::function<int(std::set<int>)> func;
    std::vector<std::set<int>> matrix(n + 1);
    std::vector<int> result(n + 1, -1);


    for (int i = 0; i < m; i++) {
        int a, b;
        std::cin >> a >> b;
        init(matrix, a, b);
    }

    std::deque<meta> deque;
    size_t deg = 0;

    for (int i = 1; i <= n; i++) {
        if (deg < matrix[i].size()) deg = matrix[i].size();
    }
    //std::sort(deque.begin(), deque.end(), meta::comp());
    if (!(deg % 2)) deg++;
    std::vector<int> kostil(deg + 1);
    deque.push_back(meta(1));
    result[deque.front().num] = 1;

    int col = 1;
    while (!deque.empty()) {
        int num = deque.front().num;
        deque.pop_front();
        bool dop = false;
        for (int i : matrix[num]) {
            if (result[i] == -1) {
                for (int j : matrix[i]) {
                    int k = result[j] == -1 ? 0 : result[j];
                    kostil[k] = 1;
                }
                int index = 0;
                while (kostil[++index] == 1);
                result[i] = index;
                deque.push_back(meta(i));
                dop = true;
                std::fill(kostil.begin(), kostil.end(), 0);
            }
        }
    }

    std::cout << deg << "\n";
    for (int i = 0; i < n; i++) {
        std::cout << result[i + 1] << "\n";
    }

    return 0;
}